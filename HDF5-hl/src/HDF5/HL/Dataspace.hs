{-# LANGUAGE OverloadedStrings #-}
-- |
-- Description of dataspaces. They define extents of arrays in HDF
-- files and their maximum size. They are also used to define
-- selection of data to read or write.
module HDF5.HL.Dataspace
  ( -- * Dataspace definition
    Dataspace
  , dataspaceRank
  , dataspaceExtent
  , setSlabSelection
    -- ** Creation
  , getDataspace
  , createDataspaceFromExtent
  , createDataspaceFromDSpace
  , withCreateDataspaceFromExtent
  , withCreateDataspaceFromDSpace
    -- * Encoding as haskell data type
  , IsExtent(..)
  , IsDataspace(..)
  , DimRepr(..)
  , pattern UNLIMITED
    -- * Data types representing extents
  , Growable(..)
  , Extent(..)
    -- * Dimensions parser
  , ParserDim
  , parseDim
  , endOfExtent
  , runParseFromDataspace
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Functor
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import GHC.Stack

import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Unsafe.Error
import HDF5.HL.Unsafe.Encoding
import HDF5.C


----------------------------------------------------------------
-- Haskell data type
----------------------------------------------------------------

-- | HDF5 uses @Word64@ to represent size of an array. Maximum
--   possible value is used to represent unlimited extend in
--   dataspace.
class (Integral a, Eq a) => DimRepr a where
  unlimitedRepr :: a

-- | Value which is used to represent than maximum size of particular
--   dimension is unbounded.
pattern UNLIMITED :: DimRepr a => a
pattern UNLIMITED <- ((==unlimitedRepr) -> True) where UNLIMITED = unlimitedRepr

instance DimRepr Word64 where
  unlimitedRepr = coerce H5S_UNLIMITED

instance DimRepr Int where
  unlimitedRepr = -1

instance DimRepr Int64 where
  unlimitedRepr = -1



-- | Type class for values representing size of array or scalar; that
--   is product of `Word64`s. For 1D it's `Int`\/`Int64`\/`Word64`. For
--   N-dimensional array extent could be represented by some tuple and
--   in case when rank is not known statically lists could be used.
class IsDataspace a => IsExtent a where
  -- | Encode extent. This fold over all dimensions of dataset for
  --   simple and scalar extents. Null extents should return @Nothing@.
  encodeExtent :: Monoid m => a -> (Word64 -> m) -> m


-- | Type class for values representing dataspace. It could be either
--   `null` which means no data is stored. Or it could be pair is size
--   of array and maximum array size. Both are sequence of N
--   `Word64`s, zero for scalars.
--
--   If data type doesn't store maximum size: `Int`\/`Int64`\/`Word64`
--   and tuples of such types, maximum size is assumed to be same as size.
--   If one needs to set maximum size 'Growable' should be used.
class IsDataspace a where
  -- | Encode pairs for 
  encodeDataspace :: Monoid m => a -> Maybe ((Word64 -> Word64 -> m) -> m)
  -- | Parser for dataset which could be used to decode from sequence
  --   of Dims.
  decodeDataspace :: Monad m => ParserDim m a
  -- | How null dataspace should be interpreted.
  decodeNullDataspace :: Maybe a
  decodeNullDataspace = Nothing



-- | Generic extent of dataspace. It could encode all possible
--   extents.
data Extent
  = Simple [Growable Word64] -- ^ Simple dataspace.
  | Null                     -- ^ Null extent for dataset which do not
                             --   contain any actual data.
  deriving stock (Show,Eq,Ord)


instance IsDataspace Extent where
  encodeDataspace = \case
    Null        -> Nothing
    Simple dims -> encodeDataspace dims
  decodeDataspace     = Simple <$> decodeDataspace
  decodeNullDataspace = Just Null

-- | Data type which stores size and maximum size of dataspace.
data Growable a = Growable !a !a
  deriving stock (Show,Eq,Ord)

-- | Extent for scalar values. It's rank-0 extent not null extent!
instance IsExtent () where
  encodeExtent ()  = \_ -> mempty


instance IsExtent Word64 where
  encodeExtent i = \f -> f i
instance IsExtent Int64 where
  encodeExtent i = \f -> f (if i < 0 then error "Negative extent" else fromIntegral i)
instance IsExtent Int where
  encodeExtent i = \f -> f (if i < 0 then error "Negative extent" else fromIntegral i)

instance (IsExtent a, IsExtent b) => IsExtent (a,b) where
  encodeExtent (a,b) f = encodeExtent a f <> encodeExtent b f

instance IsExtent a => IsExtent [a] where
  encodeExtent xs f = foldMap (\x -> encodeExtent x f) xs


-- | Extent for scalar values. It's rank-0 extent not null extent!
instance IsDataspace () where
  encodeDataspace () = Just mempty
  decodeDataspace    = pure ()

instance IsDataspace Word64 where
  encodeDataspace i = Just $ \f ->f i i
  decodeDataspace = fst <$> parseDim

instance IsDataspace Int where
  encodeDataspace i
    | i < 0     = error "Negative size"
    | otherwise = Just $ \f -> f (fromIntegral i) (fromIntegral i)
  decodeDataspace = parseDim >>= \case
    (w,_) | w > fromIntegral (maxBound::Int) -> empty -- FIXME: pass that size is wrong!
          | otherwise                        -> pure $! fromIntegral w

instance IsDataspace Int64 where
  encodeDataspace i
    | i < 0     = error "Negative size"
    | otherwise = Just $ \f -> f (fromIntegral i) (fromIntegral i)
  decodeDataspace = parseDim >>= \case
    (w,_) | w > fromIntegral (maxBound::Int) -> empty -- FIXME: pass that size is wrong!
          | otherwise                        -> pure $! fromIntegral w

instance IsDataspace (Growable Word64) where
  encodeDataspace (Growable w m) = Just $ \f -> f w m
  decodeDataspace = uncurry Growable <$> parseDim

instance IsDataspace (Growable Int) where
  encodeDataspace = \case
    Growable w UNLIMITED
      | w < 0     -> error "Negative size"
      | otherwise -> Just $ \f -> f (fromIntegral w) UNLIMITED
    Growable w m
      | w < 0     -> error "Negative size"
      | m < 0     -> error "Negative size"
      | otherwise -> Just $ \f -> f (fromIntegral w) (fromIntegral m)
  decodeDataspace = parseDim >>= \case
    (w,UNLIMITED)
      | w > fromIntegral (maxBound::Int) -> empty
      | otherwise -> pure $! Growable (fromIntegral w) UNLIMITED
    (w,m)
      | w > fromIntegral (maxBound::Int) -> empty
      | m > fromIntegral (maxBound::Int) -> empty
      | otherwise -> pure $! Growable (fromIntegral w) (fromIntegral m)

instance IsDataspace (Growable Int64) where
  encodeDataspace = \case
    Growable w UNLIMITED
      | w < 0     -> error "Negative size"
      | otherwise -> Just $ \f -> f (fromIntegral w) UNLIMITED
    Growable w m
      | w < 0     -> error "Negative size"
      | m < 0     -> error "Negative size"
      | otherwise -> Just $ \f -> f (fromIntegral w) (fromIntegral m)
  decodeDataspace = parseDim >>= \case
    (w,UNLIMITED)
      | w > fromIntegral (maxBound::Int64) -> empty
      | otherwise -> pure $! Growable (fromIntegral w) UNLIMITED
    (w,m)
      | w > fromIntegral (maxBound::Int64) -> empty
      | m > fromIntegral (maxBound::Int64) -> empty
      | otherwise -> pure $! Growable (fromIntegral w) (fromIntegral m)


instance (IsDataspace a, IsDataspace b) => IsDataspace (a,b) where
  encodeDataspace (a,b) = (<>) (encodeDataspace a) (encodeDataspace b)
  decodeDataspace = liftA2 (,) decodeDataspace decodeDataspace

instance (IsDataspace a) => IsDataspace [a] where
  encodeDataspace xs = do
    fs <- traverse encodeDataspace xs
    Just $ \f -> foldMap ($ f) fs
  decodeDataspace = many decodeDataspace






----------------------------------------------------------------
-- Parser for dataspaces
----------------------------------------------------------------

-- | Very simple parser for sequence of values of type @i@
newtype ParserDim m a = ParserDim
  { unParserDim :: forall s.
                   (s -> m (Maybe (s, (Word64, Word64))))
                -> (s -> m (Maybe (s, a)))
  }
  deriving stock Functor

instance Monad m => Applicative (ParserDim m) where
  pure a = ParserDim $ \_ s -> pure (Just (s,a))
  ParserDim pf <*> ParserDim pa = ParserDim $ \uncons s -> runMaybeT $ do
    (s',  f) <- MaybeT $ pf uncons s
    (s'', a) <- MaybeT $ pa uncons s'
    pure (s'', f a)

instance Monad m => Alternative (ParserDim m) where
  empty = ParserDim $ \_ _ -> pure Nothing
  ParserDim pa <|> ParserDim pb = ParserDim $ \uncons s -> runMaybeT (MaybeT (pa uncons s) <|> MaybeT (pb uncons s))

instance Monad m => Monad (ParserDim m) where
  m >>= f = ParserDim $ \uncons s0 -> runMaybeT $ do
    (s1,a) <- MaybeT $ unParserDim m uncons s0
    MaybeT $ unParserDim (f a) uncons s1

parseDim :: ParserDim m (Word64,Word64)
parseDim = ParserDim id

endOfExtent :: Monad m => ParserDim m ()
endOfExtent = ParserDim $ \uncons s -> uncons s >>= \case
  Nothing -> pure $ Just (s,())
  Just _  -> pure Nothing

runParserDim
  :: Monad m
  => (s -> m (Maybe (s,(Word64,Word64))))
  -> s
  -> ParserDim m a
  -> m (Maybe a)
runParserDim uncons s0 (ParserDim fun) = fmap snd <$> fun uncons s0


runParseFromDataspace
  :: (IsDataspace a, MonadIO m, MonadThrow m)
  => Dataspace
  -> m (Either DataspaceParseError a)
runParseFromDataspace (getHID -> hid) = propagateError $ do
  p_err <- ContT alloca
  lift (h5s_get_simple_extent_type hid p_err) >>= \case
    H5S_NULL   -> pure $ case decodeNullDataspace of
      Just d  -> Right d
      Nothing -> Left  UnexpectedNull
    H5S_SCALAR -> runParserDim (\() -> pure Nothing) ()  decodeDataspace <&> \case
      Just a  -> Right a
      Nothing -> Left (BadRank [])
    H5S_SIMPLE -> do
      rank <- fmap fromIntegral
            $ contCheckCInt p_err "Cannot get rank of simple extent"
            $ h5s_get_simple_extent_ndims hid
      -- Allocate buffers
      p_dim <- ContT $ allocaArray rank
      p_max <- ContT $ allocaArray rank
      do _ <- contCheckCInt p_err "Cannot get extent for simple dataspace"
            $ h5s_get_simple_extent_dims hid p_dim p_max
         --
         let uncons i | i >= rank = pure Nothing
                      | otherwise = do
                          dim  <- peekElemOff p_dim i
                          mdim <- peekElemOff p_max i
                          pure $ Just (i+1, (dim, mdim))
         liftIO $ runParserDim uncons 0 decodeDataspace >>= \case
           Just a  -> pure (Right a)
           Nothing -> do
             dim  <- peekArray rank p_dim
             dmax <- peekArray rank p_max
             pure $ Left $ BadRank $ zip dim dmax
    _ -> abort p_err "Cannot get class of dataspace"




----------------------------------------------------------------
-- Dataspace creation
----------------------------------------------------------------

-- | Create dataspace for a given extent. This only creates scalar or
--   simple dataspaces with maximum size same as real size.
createDataspaceFromExtent
  :: (IsExtent dim, MonadIO m, MonadThrow m, HasCallStack)
  => dim       -- ^ Extent of dataspace
  -> m Dataspace
createDataspaceFromExtent dim = propagateError $ do
  p_err      <- ContT $ alloca
  (rank,ptr) <- withEncodedExtent $ encodeExtent dim
  fmap Dataspace
    $ contCheckHID p_err "Unable to create simple dataspace"
    $ h5s_create_simple (fromIntegral rank) ptr nullPtr


-- | Create dataspace for a given extent. This variant allow creation
--   of all possible dataspaces.
createDataspaceFromDSpace
  :: (IsDataspace dim, MonadIO m, MonadThrow m, HasCallStack)
  => dim       -- ^ Extent of dataspace
  -> m Dataspace
createDataspaceFromDSpace dspace = propagateError $ do
  p_err <- ContT $ alloca
  case encodeDataspace dspace of
    Nothing -> fmap Dataspace
             $ contCheckHID p_err "Unable to create dataspace with NULL extent"
             $ h5s_create H5S_NULL
    Just encoder -> do
      (rank,p_sz,p_max) <- withEncodedDataspace encoder
      fmap Dataspace
        $ contCheckHID p_err "Unable to create simple dataspace"
        $ h5s_create_simple (fromIntegral rank) p_sz p_max


-- | Bracket wrapping 'createDataspaceFromExtent'
withCreateDataspaceFromExtent
  :: IsExtent dim
  => dim                 -- ^ Extent of dataspace
  -> (Dataspace -> IO a) -- ^ Continuation
  -> IO a
withCreateDataspaceFromExtent dim
  = bracket (createDataspaceFromExtent dim) basicClose

-- | Bracket wrapping 'createDataspaceFromDSpace'
withCreateDataspaceFromDSpace
  :: IsDataspace dim
  => dim                 -- ^ Extent of dataspace
  -> (Dataspace -> IO a) -- ^ Continuation
  -> IO a
withCreateDataspaceFromDSpace dim
  = bracket (createDataspaceFromDSpace dim) basicClose


-- | Set selection in dataspace to a regular slab.
setSlabSelection
  :: (IsExtent dim, MonadIO m, MonadThrow m)
  => Dataspace
  -> dim        -- ^ Offset
  -> dim        -- ^ Size of selection
  -> m ()
setSlabSelection (Dataspace hid) off sz = propagateError $ do
  p_err <- ContT alloca
  --
  rank_dset <- contCheckCInt p_err "Cannot get rank of dataspace's extent"
             $ h5s_get_simple_extent_ndims hid
  --
  (rank_off, p_off) <- withEncodedExtent $ encodeExtent off
  (rank_sz , p_sz)  <- withEncodedExtent $ encodeExtent sz
  when (rank_off /= rank_sz) $ throwM $
    Error "In dataspace selection ranks of an offset and size do not match" []
  when (fromIntegral rank_dset /= rank_sz) $ throwM $
    Error "Rank of size does not match rank of dataset" []
  contCheckHErr p_err "Unable to set simple hyperslab selection"
    $ h5s_select_hyperslab hid H5S_SELECT_SET
        p_off nullPtr
        p_sz  nullPtr
  pure ()


----------------------------------------------------------------
-- Dataspace querying
----------------------------------------------------------------

-- | Return dataspace associated with dataset or attribute.
getDataspace :: (HasData a, MonadIO m, HasCallStack) => a -> m Dataspace
getDataspace = liftIO . getDataspaceIO

-- | Find rank of dataspace. Returns @Nothing@ for null dataspaces,
--   @Just 0@ for scalars and @Just n@ for rank-N arrays.
dataspaceRank
  :: (HasCallStack, MonadIO m, MonadThrow m)
  => Dataspace
  -> m (Maybe Int)
dataspaceRank (Dataspace hid) = propagateError $ do
  p_err <- ContT alloca
  lift (h5s_get_simple_extent_type hid p_err) >>= \case
      H5S_NULL   -> pure   Nothing
      H5S_SCALAR -> pure $ Just 0
      H5S_SIMPLE -> do
        n <- contCheckCInt p_err "Cannot get rank of dataspace's extent"
           $ h5s_get_simple_extent_ndims hid
        pure $ Just (fromIntegral n)
      _ -> abort p_err "Cannot get dataspace type"

-- | Parse extent of dataspace. Returns @Nothing@ if dataspace doens't
--   match expected shape.
dataspaceExtent
  :: (MonadIO m, IsDataspace ext, HasCallStack)
  => Dataspace
  -> m (Either DataspaceParseError ext)
dataspaceExtent spc = liftIO $ runParseFromDataspace spc
