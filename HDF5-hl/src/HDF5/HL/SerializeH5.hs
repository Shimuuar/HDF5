{-# LANGUAGE UndecidableInstances #-}
-- |
module HDF5.HL.SerializeH5
  ( -- * Serialization machinery
    H5Reader
  , H5Writer
  , H5Serialize(..)
  , H5Serialize1(..)
  , h5Read1
  , h5Write1
  , FilePathRepr(..)
    -- * Deriving via
  , ViaDataset(..)
  , ViaSerialize1(..)
  , ReprViaShowRead(..)
  ) where

import Control.Monad
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Int
import Data.IntMap.Strict           qualified as IntMap
import Data.Map.Strict              qualified as Map
import Data.Proxy
import Data.Text                    qualified as T
import Data.Text.Lazy               qualified as TL
import Data.Traversable
import Data.Vector                  qualified as V
import Data.Vector.Fixed            qualified as F
import Data.Vector.Fixed.Boxed      qualified as FB
import Data.Vector.Fixed.Primitive  qualified as FP
import Data.Vector.Fixed.Storable   qualified as FS
import Data.Vector.Fixed.Strict     qualified as FV
import Data.Vector.Fixed.Unboxed    qualified as FU
import Data.Vector.Primitive        qualified as VP
import Data.Vector.Storable         qualified as VS
import Data.Vector.Strict           qualified as VV
import Data.Vector.Unboxed          qualified as VU
import Data.Word

import GHC.Generics
import GHC.TypeLits
import GHC.Stack

import HDF5.HL        qualified as H5
import HDF5.HL.Vector (VecHDF5)

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Function which reads object at location given by @FilePath@
--   parameter relative to @dir@ (could be HDF5 file or group inside it).
type H5Reader a = forall dir. (HasCallStack, H5.IsDirectory dir) => dir -> FilePath -> IO a

-- | Function which writes object at location given by @FilePath@
--   parameter relative to @dir@ (could be HDF5 file or group inside it).
type H5Writer a = forall dir. (HasCallStack, H5.IsDirectory dir) => dir -> FilePath -> a -> IO ()

-- | Type class for values that could be serialized as some collection
--   of HDF5 datasets in possibly arbitrary tree.
class H5Serialize a where
  -- | Read value from given path.
  h5Read  :: H5Reader a
  -- | Write value at given path.
  h5Write :: H5Writer a

-- | Lifted variant of 'H5Serialize'.
class H5Serialize1 f where
  liftH5Read  :: H5Reader a -> H5Reader (f a)
  liftH5Write :: H5Writer a -> H5Writer (f a)

h5Read1 :: (H5Serialize1 f, H5Serialize a) => H5Reader (f a)
h5Read1 = liftH5Read h5Read

h5Write1 :: (H5Serialize1 f, H5Serialize a) => H5Writer (f a)
h5Write1 = liftH5Write h5Write


-- | Type class for values that could be represented as path fragments
class FilePathRepr a where
  toFilePath   :: a -> FilePath
  fromFilePath :: FilePath -> Maybe a

instance FilePathRepr [Char] where
  toFilePath   = id
  fromFilePath = Just
instance FilePathRepr T.Text where
  toFilePath   = T.unpack
  fromFilePath = Just . T.pack
instance FilePathRepr TL.Text where
  toFilePath   = TL.unpack
  fromFilePath = Just . TL.pack

deriving via ReprViaShowRead Int    instance FilePathRepr Int
deriving via ReprViaShowRead Int8   instance FilePathRepr Int8
deriving via ReprViaShowRead Int16  instance FilePathRepr Int16
deriving via ReprViaShowRead Int32  instance FilePathRepr Int32
deriving via ReprViaShowRead Int64  instance FilePathRepr Int64
deriving via ReprViaShowRead Word   instance FilePathRepr Word
deriving via ReprViaShowRead Word8  instance FilePathRepr Word8
deriving via ReprViaShowRead Word16 instance FilePathRepr Word16
deriving via ReprViaShowRead Word32 instance FilePathRepr Word32
deriving via ReprViaShowRead Word64 instance FilePathRepr Word64


-- | Derive 'FilePathRepr' using @Show/Read@ pair.
newtype ReprViaShowRead a = ReprViaShowRead a

instance (Show a, Read a) => FilePathRepr (ReprViaShowRead a) where
  toFilePath (ReprViaShowRead a) = show a
  fromFilePath s = case reads s of
    [(a,"")] -> Just (ReprViaShowRead a)
    _        -> Nothing


----------------------------------------------------------------
-- Deriving via
----------------------------------------------------------------

-- | Derive 'H5Serialize' instance for data type which has
--   'HDF5.HL.SerializeDSet' instance.
newtype ViaDataset a = ViaDataset a

instance H5.SerializeDSet a => H5Serialize (ViaDataset a) where
  h5Read  dir path
    = ViaDataset <$> H5.readDatasetAt dir path
  h5Write dir path (ViaDataset a)
    = H5.writeDatasetAt dir path a

-- | Derive 'H5Serialize' for data types which has 'H5Serialize1' instnace
newtype ViaSerialize1 f a = ViaSerialize1 (f a)

instance (H5Serialize a, H5Serialize1 f) => H5Serialize (ViaSerialize1 f a) where
  h5Read  dir = coerce (liftH5Read  @f (h5Read  @a) dir)
  h5Write dir = coerce (liftH5Write @f (h5Write @a) dir)


-- | Derive representation for records which puts each field into its
--   own dataset\/group.
instance (Generic a, GSerializeLoc (Rep a)) => H5Serialize (Generically a) where
  h5Read dir path
    = H5.withOpenGroup dir path $ fmap (Generically . to) . gH5Read
  h5Write dir path (Generically a)
    = H5.withCreateGroup dir path $ \g -> gH5Write g (from a)

class GSerializeLoc f where
  gH5Read  :: (HasCallStack) => H5.Group -> IO (f p)
  gH5Write :: (HasCallStack) => H5.Group -> f p -> IO ()

deriving newtype instance GSerializeLoc f => GSerializeLoc (M1 D i f)
deriving newtype instance GSerializeLoc f => GSerializeLoc (M1 C i f)

instance (GSerializeLoc f, GSerializeLoc g) => GSerializeLoc (f :*: g) where
  gH5Read  dir = (:*:) <$> gH5Read dir <*> gH5Read dir
  gH5Write dir (f :*: g) = gH5Write dir f >> gH5Write dir g

instance ( H5Serialize a
         , KnownSymbol fld
         ) => GSerializeLoc (M1 S ('MetaSel ('Just fld) su ss ds) (K1 i a)) where
  gH5Read dir = M1 . K1 <$> h5Read dir path where
    path = symbolVal $ Proxy @fld
  gH5Write dir (M1 (K1 a)) = h5Write dir path a where
    path = symbolVal $ Proxy @fld

instance ( TypeError ('Text "Unable to serialize sum types to HDF5")
         ) => GSerializeLoc (f :+: g) where
  gH5Read  = error "UNREACHABLE"
  gH5Write = error "UNREACHABLE"

instance ( TypeError ('Text "All record fields must be named")
         ) => GSerializeLoc (M1 S ('MetaSel 'Nothing su ss ds) (K1 i a)) where
  gH5Read  = error "UNREACHABLE"
  gH5Write = error "UNREACHABLE"



----------------------------------------------------------------
-- Instances for H5Serialize
----------------------------------------------------------------

deriving via ViaDataset Int    instance H5Serialize Int
deriving via ViaDataset Int8   instance H5Serialize Int8
deriving via ViaDataset Int16  instance H5Serialize Int16
deriving via ViaDataset Int32  instance H5Serialize Int32
deriving via ViaDataset Int64  instance H5Serialize Int64
deriving via ViaDataset Word   instance H5Serialize Word
deriving via ViaDataset Word8  instance H5Serialize Word8
deriving via ViaDataset Word16 instance H5Serialize Word16
deriving via ViaDataset Word32 instance H5Serialize Word32
deriving via ViaDataset Word64 instance H5Serialize Word64

deriving via ViaDataset [a]           instance H5.Element a                  => H5Serialize [a]
deriving via ViaDataset (V.Vector  a) instance H5.Element a                  => H5Serialize (V.Vector  a)
deriving via ViaDataset (VS.Vector a) instance (H5.Element a, VS.Storable a) => H5Serialize (VS.Vector a)
deriving via ViaDataset (VU.Vector a) instance (H5.Element a, VU.Unbox a)    => H5Serialize (VU.Vector a)
deriving via ViaDataset (VP.Vector a) instance (H5.Element a, VP.Prim a)     => H5Serialize (VP.Vector a)
deriving via ViaDataset (VV.Vector a) instance (H5.Element a)                => H5Serialize (VV.Vector a)
deriving via ViaDataset (VecHDF5   a) instance (H5.Element a)                => H5Serialize (VecHDF5 a)

deriving via ViaDataset (FU.Vec n a)
    instance (H5.Element a, FU.Unbox n a) => H5Serialize (FU.Vec n a)
deriving via ViaDataset (FB.Vec n a)
    instance (H5.Element a, F.Arity n) => H5Serialize (FB.Vec n a)
deriving via ViaDataset (FV.Vec n a)
    instance (H5.Element a, F.Arity n) => H5Serialize (FV.Vec n a)
deriving via ViaDataset (FS.Vec n a)
    instance (H5.Element a, F.Arity n, FS.Storable a) => H5Serialize (FS.Vec n a)
deriving via ViaDataset (FP.Vec n a)
    instance (H5.Element a, F.Arity n, FP.Prim a) => H5Serialize (FP.Vec n a)

deriving newtype instance H5Serialize a => H5Serialize (Identity a)
deriving via ViaSerialize1 IntMap.IntMap a
    instance H5Serialize a => H5Serialize (IntMap.IntMap a)
deriving via ViaSerialize1 (Map.Map k) a
    instance (H5Serialize a, FilePathRepr k, Ord k) => H5Serialize (Map.Map k a)


----------------------------------------------------------------
-- Instances for H5Serialize1
----------------------------------------------------------------

instance H5Serialize1 Identity where
  liftH5Read  reader dir path = Identity <$> reader dir path
  liftH5Write writer dir path = writer dir path . runIdentity

instance (H5Serialize1 f, H5Serialize1 g) => H5Serialize1 (f `Compose` g) where
  liftH5Read reader dir path
    = Compose <$> liftH5Read (liftH5Read reader) dir path
  liftH5Write writer dir path (Compose a)
    = liftH5Write (liftH5Write writer) dir path a


instance H5Serialize1 IntMap.IntMap where
  liftH5Write writer dir path xs =
    H5.withCreateGroup dir path $ \d -> do
      forM_ (IntMap.toList xs) $ \(k,v) ->
        writer d (toFilePath k) v
  liftH5Read reader dir path =
    H5.withOpenGroup dir path $ \d -> do
      entries <- H5.listGroup d
      names   <- for entries $ \s -> case fromFilePath s of
        Just k  -> pure (s,k)
        Nothing -> error $ "H5Serialize1 for IntMap: can't decode " ++ s
      fmap IntMap.fromList $ for names $ \(s,k) -> do
        a <- reader d s
        pure (k,a)

instance (Ord k, FilePathRepr k) => H5Serialize1 (Map.Map k) where
  liftH5Write writer dir path xs =
    H5.withCreateGroup dir path $ \d -> do
      forM_ (Map.toList xs) $ \(k,v) ->
        writer d (toFilePath k) v
  liftH5Read reader dir path =
    H5.withOpenGroup dir path $ \d -> do
      entries <- H5.listGroup d
      names   <- for entries $ \s -> case fromFilePath s of
        Just k  -> pure (s,k)
        Nothing -> error $ "H5Serialize1 for Map: can't decode " ++ s
      fmap Map.fromList $ for names $ \(s,k) -> do
        a <- reader d s
        pure (k,a)
