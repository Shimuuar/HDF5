{-# LANGUAGE CPP #-}
-- |
module HDF5.HL.Unsafe.Property
  ( -- * Property lists
    Property(..)
    -- * Dataset properties
  , withDatasetProps
  , propDatasetLayout
  , propDatasetChunking
  , propDatasetDeflate
  ) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative(liftA2)
#endif
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Foreign.Ptr
import Foreign.Marshal
import GHC.Stack

import HDF5.C
import HDF5.HL.Unsafe.Enum
import HDF5.HL.Unsafe.Error
import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Dataspace
import HDF5.HL.Unsafe.Encoding


----------------------------------------------------------------
-- Property lists
----------------------------------------------------------------

-- | Property lists for values of type p. This data type is used to
--   specify properties of objects at their creation time.
data Property p = NoProperty
                | Property (Ptr HID -> PropertyHID p -> IO ())

instance Semigroup (Property p) where
  NoProperty <> p          = p
  p          <> NoProperty = p
  Property f <> Property g = Property $ (liftA2 . liftA2) (>>) f g
  
instance Monoid (Property p) where
  mempty = NoProperty


----------------------------------------------------------------
-- Dataset
----------------------------------------------------------------

withDatasetProps :: Property Dataset -> (PropertyHID Dataset -> IO a) -> IO a
withDatasetProps prop action = case prop of
  NoProperty -> action $ PropertyHID H5P_DEFAULT
  Property f -> alloca $ \p_err -> do
    let open = fmap PropertyHID
             $ checkHID p_err "Unable to create property list"
             $ h5p_create H5P_DATASET_CREATE
    bracket open basicClose $ \p -> f p_err p >> action p

-- | Set up dataset layout
propDatasetLayout :: HasCallStack => Layout -> Property Dataset
propDatasetLayout l = Property $ \p_err p -> withFrozenCallStack
  $ checkHErr p_err "Unable to set layout for dataset"
  $ h5p_set_layout (getHID p) (toCEnum l)
  
-- | Set chunking for a dataset
propDatasetChunking :: (HasCallStack, IsExtent dim) => dim -> Property Dataset
propDatasetChunking dim = Property $ \p_err prop -> withFrozenCallStack $ evalContT $ do
  (rank,p) <- withEncodedExtent $ encodeExtent dim
  lift $ checkHErr p_err "Unable to set chunk size"
       $ h5p_set_chunk (getHID prop) (fromIntegral rank) p

-- | Use gzip compression for dataset. Compression level is specified
--   by number. 0 is no compression (but compression filter is still
--   present!) and 9 is highest.
propDatasetDeflate
  :: HasCallStack
  => Int -- ^ Compression level. Out of range values are clamped
  -> Property Dataset
propDatasetDeflate lvl = Property $ \p_err prop -> withFrozenCallStack
  $ checkHErr p_err "Unable to set compression level"
  $ h5p_set_deflate (getHID prop) z
  where
    z | lvl < 0   = 0
      | lvl > 9   = 9
      | otherwise = fromIntegral lvl
  
