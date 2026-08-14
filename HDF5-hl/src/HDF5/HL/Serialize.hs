{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- All modules inside @HDF5.HL.Internal@ constitute internal API.
-- It considered part of public API but its stability is of little
-- concert and shouldn't be relied upon.
module HDF5.HL.Serialize
  ( -- * Array serialization
    ArrayLike(..)
  , readAll
  , writeAll
  , readSlab
  , writeSlab
    -- * Dataset serialization
  , SerializeDSet(..)
    -- * Deriving via
  , SerializeAsScalar(..)
  , SerializeAsArray(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Functor.Identity
import Data.Complex                (Complex)
import Data.Vector                 qualified as V
import Data.Vector.Storable        qualified as VS
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Generic         qualified as VG
import Data.Vector.Strict          qualified as VV
import Data.Vector.Primitive       qualified as VP
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP
import Data.Vector.Fixed.Strict    qualified as FV
import Control.Monad.Trans.Cont
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.ForeignPtr
import Data.Int
import Data.Word
import GHC.Stack

import HDF5.HL.Unsafe.Types
import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Unsafe.Error
import HDF5.HL.Dataspace
import HDF5.HL.Unsafe.Property
import HDF5.HL.Vector
import HDF5.HL.Monad
import HDF5.C
import Prelude hiding (read,readIO)


----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

-- | Reads full content of dataset\/attribute into haskell data
--   structure.
readAll
  :: forall a d m. (ArrayLike a, HasData d, MonadIO m, MonadMask m, HasCallStack)
  => d -> m a
readAll dset = runLiftHdf5M $ do
  spc_file <- getDataspaceHDF dset
  ty_a     <- typeH5 @(ElementOf a)
  ext      <- dataspaceExtent @_ @(ExtentOf a) spc_file >>= \case
    Left  e -> throwM e -- FIXME: wrong throwing method!
    Right x -> pure x
  (ptr,mkA) <- basicReadFromSlab ext
  unsafeReadAll dset ty_a ptr
  liftIO mkA

-- | Writing into dataset\/attributes without offset. It's assumed
--   that dataset was created with correct size.
writeAll
  :: forall a d m. (ArrayLike a, HasData d, MonadIO m, MonadThrow m, HasCallStack)
  => d -- ^ Dataset or attribute
  -> a -- ^ Value to write
  -> m ()
writeAll dset a = runLiftHdf5M $ do
  ty_a <- typeH5 @(ElementOf a)
  ptr  <- basicWriteToSlab a
  unsafeWriteAll dset ty_a ptr

-- | Read data from dataset using slab selection. For example:
--
-- > readSlab dset (100::Int) (30::Int)
--
-- will read 30 element at offset 100.
readSlab
  :: forall a m. (ArrayLike a, MonadIO m, MonadMask m, HasCallStack)
  => Dataset    -- ^ Dataset to read from
  -> ExtentOf a -- ^ Offset into array
  -> ExtentOf a -- ^ Array size
  -> m a
readSlab d off sz = runLiftHdf5M $ do
  spc_file   <- getDataspaceHDF d
  ty_a       <- typeH5 @(ElementOf a)
  (ptr, mkA) <- basicReadFromSlab sz
  liftIO $ setSlabSelection spc_file off sz
  spc_mem <- hdfCreateDataspaceFromExtent sz
  contCheckHErr "Reading dataset data failed"
    $ h5d_read (getHID d) (getTypeHID ty_a)
        (getHID spc_mem) (getHID spc_file)
        H5P_DEFAULT (castPtr ptr)
  liftIO mkA


-- | Write provided data at given offset. For example
--
-- > writeSlab dset (100::Int) [1 .. 10::Int]
--
-- will write 10 elements at offset 100.
writeSlab
  :: forall a m. (ArrayLike a, MonadIO m, MonadThrow m, HasCallStack)
  => Dataset    -- ^ Dataset to work on
  -> ExtentOf a -- ^ Offset into array
  -> a          -- ^ Value to write
  -> m ()
writeSlab dset off a = runLiftHdf5M $ do
  spc_file <- getDataspaceHDF dset
  ty_a     <- typeH5 @(ElementOf a)
  ptr      <- basicWriteToSlab a
  liftIO $ setSlabSelection spc_file off (getExtent a)
  spc_mem  <- hdfCreateDataspaceFromExtent $ getExtent a
  contCheckHErr "Writing dataset data failed"
    $ h5d_write (getHID dset) (getTypeHID ty_a)
        (getHID spc_mem) (getHID spc_file) H5P_DEFAULT ptr


----------------------------------------------------------------
-- Type classes for reading/writing
----------------------------------------------------------------

-- | Data types which directly represent HDF5 N-dimensional arrays.
--   Usually operations are not zero-copy: library first needs to read
--   data into array and then parse it into haskell data. Writing
--   works in reverse order. Notable exception is 'VecHDF5' since it
--   uses same representation as `HDF5` library.
class (Element (ElementOf a), IsExtent (ExtentOf a)) => ArrayLike a where
  -- | Type of array element.
  type ElementOf a
  -- | Size of array. It's isomorphic to some product of `Word64`.
  type ExtentOf  a
  -- | Primitive for writing of HDF5 arrays (and scalars). This
  --   function returns pointer to data which could be use by C
  --   functions.
  basicWriteToSlab
    :: a                         -- ^ Value to pass
    -> Hdf5M (Ptr (ElementOf a)) -- ^ Callback consuming buffer
  -- | Primitive for reading HDF5 arrays. Function returns pointer to
  --   buffer for data to read into and IO function for converting it
  --   into result type.
  --
  --   Such API is required in order to give control of allocation to
  --   instance. Some may allocate buffer once and return pointer to
  --   it so it could be filled directly.
  basicReadFromSlab
    :: ExtentOf a -- ^ Size of an array
    -> Hdf5M (Ptr (ElementOf a), IO a)
  -- | Compute size of an array
  getExtent :: a -> ExtentOf a




-- | Data type which could be serialized as single HDF5 dataset.
--   Main difference from 'ArrayLike' is that it could use attributes.
--   Only law that instance should uphold is roundtripping.
class SerializeDSet a where
  -- | Primitive. Use 'readDatasetAt' instead.
  basicReadDSet :: Dataset -> IO a
  -- | Primitive. Use 'writeDatasetAt' instead.
  basicWriteDSet
    :: a
    -> (forall ext. IsDataspace ext => ext -> Type -> [Property Dataset] -> Hdf5M Dataset)
    -> Hdf5M ()


----------------------------------------------------------------
-- Deriving instances
----------------------------------------------------------------

type role SerializeAsScalar representational
type role SerializeAsArray  representational

-- | Newtype wrapper for derivation of serialization instances as
--   scalars.
newtype SerializeAsScalar a = SerializeAsScalar a
  deriving newtype (Storable, Element)

instance Element a => ArrayLike (SerializeAsScalar a) where
  type ElementOf (SerializeAsScalar a) = a
  type ExtentOf  (SerializeAsScalar a) = ()
  getExtent _ = ()
  basicReadFromSlab () = do
    p <- liftBracket allocaElement
    pure (p, SerializeAsScalar <$> peekH5 p)
  basicWriteToSlab (SerializeAsScalar a) = do
    ptr <- liftBracket allocaElement
    liftIO $ pokeH5 ptr a
    return ptr

deriving via SerializeAsArray (SerializeAsScalar a)
   instance Element a => SerializeDSet (SerializeAsScalar a)


-- | Default implementation of 'SerializeDSet' in terms of 'ArrayLike'.
newtype SerializeAsArray a = SerializeAsArray a

instance ArrayLike a => SerializeDSet (SerializeAsArray a) where
  basicReadDSet d = SerializeAsArray <$> readAll d
  basicWriteDSet (SerializeAsArray a) make_dset = do
    ty   <- typeH5 @(ElementOf a)
    dset <- make_dset (getExtent a) ty []
    liftIO $ writeAll dset a



----------------------------------------------------------------
-- Instance boilerplate
----------------------------------------------------------------

instance (Element a) => ArrayLike (VecHDF5 a) where
  type ElementOf (VecHDF5 a) = a
  type ExtentOf  (VecHDF5 a) = Int
  getExtent = VG.length
  basicWriteToSlab  v  = liftBracket (unsafeWithH5 v)
  basicReadFromSlab sz = do
    buf <- liftIO      $ mallocVectorH5 sz
    ptr <- liftBracket $ withForeignPtr buf
    pure ( ptr
         , pure $! unsafeFromForeignPtr buf sz )

instance (Element a) => ArrayLike [a] where
  type ElementOf [a] = a
  type ExtentOf  [a] = Int
  getExtent = length
  basicWriteToSlab v = basicWriteToSlab (VG.fromList v :: VecHDF5 a)
  basicReadFromSlab = (fmap . fmap . fmap) VG.toList . basicReadFromSlab @(VecHDF5 a)

instance (Element a) => ArrayLike (V.Vector a) where
  type ElementOf (V.Vector a) = a
  type ExtentOf  (V.Vector a) = Int
  getExtent = V.length
  basicWriteToSlab v = basicWriteToSlab (VG.convert v :: VecHDF5 a)
  basicReadFromSlab = (fmap . fmap . fmap) VG.convert . basicReadFromSlab @(VecHDF5 a)

instance (Element a) => ArrayLike (VV.Vector a) where
  type ElementOf (VV.Vector a) = a
  type ExtentOf  (VV.Vector a) = Int
  getExtent = VV.length
  basicWriteToSlab v = basicWriteToSlab (VG.convert v :: VecHDF5 a)
  basicReadFromSlab = (fmap . fmap . fmap) VG.convert . basicReadFromSlab @(VecHDF5 a)

instance (Storable a, Element a) => ArrayLike (VS.Vector a) where
  type ElementOf (VS.Vector a) = a
  type ExtentOf  (VS.Vector a) = Int
  getExtent = VS.length
  basicWriteToSlab v = basicWriteToSlab (VG.convert v :: VecHDF5 a)
  basicReadFromSlab = (fmap . fmap . fmap) VG.convert . basicReadFromSlab @(VecHDF5 a)

instance (VP.Prim a, Element a) => ArrayLike (VP.Vector a) where
  type ElementOf (VP.Vector a) = a
  type ExtentOf  (VP.Vector a) = Int
  getExtent = VP.length
  basicWriteToSlab v = basicWriteToSlab (VG.convert v :: VecHDF5 a)
  basicReadFromSlab = (fmap . fmap . fmap) VG.convert . basicReadFromSlab @(VecHDF5 a)

instance (VU.Unbox a, Element a) => ArrayLike (VU.Vector a) where
  type ElementOf (VU.Vector a) = a
  type ExtentOf  (VU.Vector a) = Int
  getExtent = VU.length
  basicWriteToSlab  v = basicWriteToSlab (VG.convert v :: VecHDF5 a)
  basicReadFromSlab = (fmap . fmap . fmap) VG.convert . basicReadFromSlab @(VecHDF5 a)


deriving via SerializeAsArray [a]
    instance (Element a) => SerializeDSet [a]
deriving via SerializeAsArray (VecHDF5 a)
    instance (Element a) => SerializeDSet (VecHDF5 a)
deriving via SerializeAsArray (V.Vector a)
    instance (Element a) => SerializeDSet (V.Vector a)
deriving via SerializeAsArray (VV.Vector a)
    instance (Element a) => SerializeDSet (VV.Vector a)
deriving via SerializeAsArray (VS.Vector a)
    instance (Element a, VS.Storable a) => SerializeDSet (VS.Vector a)
deriving via SerializeAsArray (VP.Vector a)
    instance (Element a, VP.Prim a) => SerializeDSet (VP.Vector a)
deriving via SerializeAsArray (VU.Vector a)
    instance (Element a, VU.Unbox a) => SerializeDSet (VU.Vector a)


deriving via SerializeAsScalar Int    instance ArrayLike     Int
deriving via SerializeAsScalar Int    instance SerializeDSet Int
deriving via SerializeAsScalar Int8   instance ArrayLike     Int8
deriving via SerializeAsScalar Int8   instance SerializeDSet Int8
deriving via SerializeAsScalar Int16  instance ArrayLike     Int16
deriving via SerializeAsScalar Int16  instance SerializeDSet Int16
deriving via SerializeAsScalar Int32  instance ArrayLike     Int32
deriving via SerializeAsScalar Int32  instance SerializeDSet Int32
deriving via SerializeAsScalar Int64  instance ArrayLike     Int64
deriving via SerializeAsScalar Int64  instance SerializeDSet Int64
deriving via SerializeAsScalar Word   instance ArrayLike     Word
deriving via SerializeAsScalar Word   instance SerializeDSet Word
deriving via SerializeAsScalar Word8  instance ArrayLike     Word8
deriving via SerializeAsScalar Word8  instance SerializeDSet Word8
deriving via SerializeAsScalar Word16 instance ArrayLike     Word16
deriving via SerializeAsScalar Word16 instance SerializeDSet Word16
deriving via SerializeAsScalar Word32 instance ArrayLike     Word32
deriving via SerializeAsScalar Word32 instance SerializeDSet Word32
deriving via SerializeAsScalar Word64 instance ArrayLike     Word64
deriving via SerializeAsScalar Word64 instance SerializeDSet Word64

deriving via SerializeAsScalar Float  instance ArrayLike     Float
deriving via SerializeAsScalar Float  instance SerializeDSet Float
deriving via SerializeAsScalar Double instance ArrayLike     Double
deriving via SerializeAsScalar Double instance SerializeDSet Double

deriving via SerializeAsScalar (Complex a)
    instance Element a => ArrayLike (Complex a)
deriving via SerializeAsScalar (Complex a)
    instance Element a => SerializeDSet (Complex a)

deriving via SerializeAsScalar (FB.Vec n a)
    instance (F.Arity n, Element a) => ArrayLike (FB.Vec n a)
deriving via SerializeAsScalar (FU.Vec n a)
    instance (F.Arity n, Element a, FU.Unbox n a) => ArrayLike (FU.Vec n a)
deriving via SerializeAsScalar (FS.Vec n a)
    instance (F.Arity n, Element a, Storable a) => ArrayLike (FS.Vec n a)
deriving via SerializeAsScalar (FP.Vec n a)
    instance (F.Arity n, Element a, FP.Prim a) => ArrayLike (FP.Vec n a)
deriving via SerializeAsScalar (FV.Vec n a)
    instance (F.Arity n, Element a) => ArrayLike (FV.Vec n a)

deriving via SerializeAsScalar (FB.Vec n a)
    instance (F.Arity n, Element a) => SerializeDSet (FB.Vec n a)
deriving via SerializeAsScalar (FU.Vec n a)
    instance (F.Arity n, Element a, FU.Unbox n a) => SerializeDSet (FU.Vec n a)
deriving via SerializeAsScalar (FS.Vec n a)
    instance (F.Arity n, Element a, Storable a) => SerializeDSet (FS.Vec n a)
deriving via SerializeAsScalar (FP.Vec n a)
    instance (F.Arity n, Element a, FP.Prim a) => SerializeDSet (FP.Vec n a)
deriving via SerializeAsScalar (FV.Vec n a)
    instance (F.Arity n, Element a) => SerializeDSet (FV.Vec n a)

deriving newtype instance ArrayLike     a => ArrayLike     (Identity a)
deriving newtype instance SerializeDSet a => SerializeDSet (Identity a)
