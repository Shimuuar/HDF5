-- |
-- Function for writing haskell data types into C buffers in order to
-- pass them into HDF5. As unsafe as usual C interactions go
module HDF5.HL.Internal.Encoding
  ( withEncodedExtent
  , withEncodedDataspace
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Coerce
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import HDF5.C
import HDF5.HL.Error
import HDF5.HL.Monad


-- | Encode rank and dimensions of an array.
withEncodedExtent
  :: ((Word64 -> DSpaceWriter) -> DSpaceWriter)
  -> Hdf5M s (Int, Ptr HSize)
withEncodedExtent encoder = liftIO $ do
  allocaArray maxRank $ \ptr -> do
    rank <- write ptr maxRank 0
    pure (rank, ptr)
  where DSpaceWriter write = encoder putDimension

-- | Encode rank, dimensions and maximum dimensions of an array
withEncodedDataspace
  :: ((Word64 -> Word64 -> DSpaceWriter) -> DSpaceWriter)
  -> Hdf5M s (Int, Ptr HSize, Ptr HSize)
withEncodedDataspace encoder = liftIO $ do
  allocaArray (maxRank * 2) $ \ptr -> do
    rank <- write ptr maxRank 0
    pure ( rank
         , ptr
         , ptr `plusPtr` (maxRank * sizeOf (undefined :: HSize)))
  where DSpaceWriter write = encoder putDimension2


----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- | Monoid which is used to fill buffers for calling HDF5 functions.
newtype DSpaceWriter = DSpaceWriter
  (  Ptr HSize -- Pointer to size
  -> Int       -- Maximum offset
  -> Int       -- Offset
  -> IO Int
  )

instance Semigroup DSpaceWriter where
  {-# INLINE (<>) #-}
  DSpaceWriter f <> DSpaceWriter g = DSpaceWriter $ \p_sz i_max ->
    g p_sz i_max <=< f p_sz i_max

instance Monoid DSpaceWriter where
  {-# INLINE mempty #-}
  mempty = DSpaceWriter $ \_ _ -> pure


putDimension :: Word64 -> DSpaceWriter
putDimension sz = DSpaceWriter go where
  go p_sz i_max i
    | i >= i_max = throwM $ Error "Internal error: buffer overrun" []
    | otherwise  = do pokeElemOff p_sz i (coerce sz)
                      pure $! i + 1

putDimension2 :: Word64 -> Word64 -> DSpaceWriter
putDimension2 sz max_sz = DSpaceWriter go where
  go p_sz i_max i
    | i >= i_max = throwM $ Error "Internal error: buffer overrun" []
    | otherwise  = do pokeElemOff p_sz i             (coerce sz)
                      pokeElemOff p_sz (maxRank + i) (coerce max_sz)
                      pure $! i + 1


-- Maximum possible rank of an array (as of HDF5 1.8)
maxRank :: Int
maxRank = 32
