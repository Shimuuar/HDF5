{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- API for working with HDF5 data types. We treat them as immutable
-- while they're mutable in HDF5. This module contains unsafe functions.
module HDF5.HL.Unsafe.Types
  ( -- * Operations on types
    Type(..)
  , getTypeHID
  , showType
  , sizeOfH5
    -- * Scalar data types
  , tyI8, tyI16, tyI32, tyI64
  , tyU8, tyU16, tyU32, tyU64
  , tyF32, tyF64
  , tyI8LE, tyI16LE, tyI32LE, tyI64LE
  , tyU8LE, tyU16LE, tyU32LE, tyU64LE
  , tyI8BE, tyI16BE, tyI32BE, tyI64BE
  , tyU8BE, tyU16BE, tyU32BE, tyU64BE
    -- * Patterns
  -- , pattern Array
  , makePackedRecord
  , makeEnumeration
    -- * Element
  , Element(..)
  , peekElemOffH5
  , peekByteOffH5
  , pokeElemOffH5
  , pokeByteOffH5
  , allocaElement
  , advancePtrH5
  , mallocVectorH5
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Bits                  (finiteBitSize)
import Data.IORef
import Data.Complex
import Data.Proxy
import Data.Int
import Data.Word
import Data.Functor.Identity
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP
import Data.Vector.Fixed.Strict    qualified as FV
import Data.Vector.Fixed.Mono      qualified as FM
import Foreign.Marshal             (alloca, allocaArray, allocaArray0, withArray, peekArray,
                                    allocaBytesAligned
                                   )
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe
import GHC.Exts        (keepAlive#)
import GHC.Stack
import GHC.Generics
import GHC.TypeLits
import GHC.ForeignPtr  (mallocPlainForeignPtrAlignedBytes)

import HDF5.HL.Unsafe.Error
import HDF5.HL.Monad
import HDF5.C

----------------------------------------------------------------
-- Type definition
----------------------------------------------------------------

-- | HDF5 data type. It's used to describe how data is laid out both
--   in memory and on disc. Type class 'Element' is used to associate
--   HDF5 type to haskell values.
data Type
  = Type   !HID -- ^ Type which should be closed
  | Native !HID -- ^ Data types which does not need to be finalized


getTypeHID :: Type -> HID
getTypeHID = \case
  Type   tid -> tid
  Native tid -> tid

instance Closable Type where
  basicClose = \case
    Native _   -> pure ()
    Type   tid -> alloca $ \p_err -> do
      -- FIXME: We don't check for errors here.
      _ <- lockHDF5 $ h5t_close tid p_err
      return ()

showType :: Type -> Hdf5M s String
showType (getTypeHID -> tid) = do
  p_sz  <- liftBracket $ alloca
  _     <- contCheckHErr "Can't show type"
         $ h5lt_dtype_to_text tid nullPtr h5lt_DDL p_sz
  sz    <- liftIO $ peek p_sz
  p_str <- liftBracket $ allocaArray0 $ fromIntegral sz
  contCheckHErr "Can't show type"
    $ h5lt_dtype_to_text tid p_str h5lt_DDL p_sz
  liftIO $ peekCString p_str


-- | Compute size of HDF5 type.
sizeOfH5 :: HasCallStack => Type -> Hdf5M s Int
sizeOfH5 ty = withFrozenCallStack $ do
  sz <- contCheckCSize "Cannot compute size of data type" 
      $ h5t_get_size (getTypeHID ty)
  pure $! fromIntegral sz


----------------------------------------------------------------
--
----------------------------------------------------------------

tyI8,tyI16,tyI32,tyI64 :: Type
tyI8  = Native h5t_NATIVE_SCHAR
tyI16 = Native h5t_NATIVE_SHORT
tyI32 = Native h5t_NATIVE_INT
tyI64 = Native h5t_NATIVE_LONG

tyU8,tyU16,tyU32,tyU64 :: Type
tyU8  = Native h5t_NATIVE_UCHAR
tyU16 = Native h5t_NATIVE_USHORT
tyU32 = Native h5t_NATIVE_UINT
tyU64 = Native h5t_NATIVE_ULONG

tyF32,tyF64 :: Type
tyF32 = Native h5t_NATIVE_FLOAT
tyF64 = Native h5t_NATIVE_DOUBLE

tyI8LE,tyI16LE,tyI32LE,tyI64LE :: Type
tyI8LE  = Native h5t_STD_I8LE
tyI16LE = Native h5t_STD_I16LE
tyI32LE = Native h5t_STD_I32LE
tyI64LE = Native h5t_STD_I64LE

tyU8LE,tyU16LE,tyU32LE,tyU64LE :: Type
tyU8LE  = Native h5t_STD_U8LE
tyU16LE = Native h5t_STD_U16LE
tyU32LE = Native h5t_STD_U32LE
tyU64LE = Native h5t_STD_U64LE

tyI8BE,tyI16BE,tyI32BE,tyI64BE :: Type
tyI8BE  = Native h5t_STD_I8BE
tyI16BE = Native h5t_STD_I16BE
tyI32BE = Native h5t_STD_I32BE
tyI64BE = Native h5t_STD_I64BE

tyU8BE,tyU16BE,tyU32BE,tyU64BE :: Type
tyU8BE  = Native h5t_STD_U8BE
tyU16BE = Native h5t_STD_U16BE
tyU32BE = Native h5t_STD_U32BE
tyU64BE = Native h5t_STD_U64BE

-- -- | @Array ty dim@ is an array of HDF5 values of type @ty@ and
-- --   dimensions @dim@.
-- pattern Array :: Type -> [Int] -> Type
-- pattern Array ty dim <- (matchArray -> Just (ty, dim))
--   where
--     Array ty dim = makeArray ty dim

-- matchArray :: Type -> Maybe (Type, [Int])
-- matchArray ty = unsafePerformIO $ runHdf5M $ do
--   tid   <- liftBracket $ withType ty
--   p_err <- askPErr
--   -- FIXME: Another error handler
--   liftIO (lockHDF5 $ h5t_get_class tid p_err) >>= \case
--     H5T_NO_CLASS -> abort "INTERNAL: Unable to get class for a type"
--     H5T_ARRAY    -> do
--         n     <- fmap fromIntegral
--                $ contCheckCInt "INTERNAL: Unable to get number of array dimensions"
--                $ h5t_get_array_ndims tid
--         buf   <- liftBracket $ allocaArray n
--         _     <- contCheckCInt "INTERNAL: Unable to get array's dimensions"
--                $ h5t_get_array_dims tid buf
--         super <- contCheckHID "INTERNAL: Cannot get supertype"
--                $ h5t_get_super tid
--         super' <- liftIO $ unsafeNewType $ pure super
--         dims  <- liftIO $ peekArray n buf
--         pure $ Just (super', fromIntegral <$> dims)
--     _ -> pure Nothing


makeArray :: Type -> [Int] -> Hdf5M s Type
makeArray ty dim = do
  p_dim <- liftBracket $ withArray (fromIntegral <$> dim)
  boundCheckHID "Cannot create array type" Type
    $ h5t_array_create (getTypeHID ty) n p_dim
  where
    n = fromIntegral $ length dim

-- | Create record with named fields.
makePackedRecord :: HasCallStack => [(String,Type)] -> Hdf5M s Type
makePackedRecord fields = do
  -- Compute size and offsets of fields
  let computeOff !off []             = pure (off,[])
      computeOff !off ((nm,ty):rest) = do
        sz           <- sizeOfH5 ty
        (size,rest') <- computeOff (off + sz) rest
        pure (size, (nm,ty,off) : rest')
  (size, fields_sz) <- computeOff 0 fields
  -- Construct type
  ty_rec <- boundCheckHID "Cannot create compound type" Type
          $ h5t_create H5T_COMPOUND (fromIntegral size)
  forM_ fields_sz $ \(nm,ty,off) -> do
    c_nm <- liftBracket $ withCString nm
    contCheckHErr "Unable to pack data types"
      $ h5t_insert (getTypeHID ty_rec) c_nm (fromIntegral off) (getTypeHID ty)
  pure ty_rec


-- | Make enumeration data type which is based on underlying type @a@.
makeEnumeration :: forall a s. (Element a, HasCallStack) => [(String,a)] -> Hdf5M s Type
makeEnumeration elems = do
  p_val    <- liftBracket allocaElement
  base_ty  <- typeH5 @a
  let base_tid = getTypeHID base_ty
  -- Create enumeration data type. We want to release it in case of
  -- any exception  
  tid <- boundCheckHID "Cannot create enumeration type" Type
       $ h5t_enum_create base_tid
  forM_ elems $ \(nm,a) -> do
    liftIO $ pokeH5 p_val a
    c_nm <- liftBracket $ withCString nm
    contCheckHErr "Cannot add member to enumeration"
      $ h5t_enum_insert (getTypeHID tid) c_nm p_val
  pure tid


----------------------------------------------------------------
-- Type class for deriving HDF5 types
----------------------------------------------------------------

-- | Data type which corresponds to some HDF data type and could read
--   from buffer using 'Storable'.
class Element a where
  -- | HDF5 type of element.
  typeH5 :: Hdf5M s Type
  -- | Compute size of element. It must be same as @sizeOfH5 (typeH5 \@a)@
  fastSizeOfH5 :: Int
  -- | Alignment requirements
  alignmentH5 :: Int
  -- | Read haskell data type from buffer
  peekH5 :: Ptr a -> IO a
  -- | Write haskell data type into buffer
  pokeH5 :: Ptr a -> a -> IO ()



peekElemOffH5 :: forall a. Element a => Ptr a -> Int -> IO a
{-# INLINE peekElemOffH5 #-}
peekElemOffH5 ptr off = peekByteOffH5 ptr (off * fastSizeOfH5 @a)

pokeElemOffH5 :: forall a. Element a => Ptr a -> Int -> a -> IO ()
{-# INLINE pokeElemOffH5 #-}
pokeElemOffH5 ptr off = pokeByteOffH5 ptr (off * fastSizeOfH5 @a)

peekByteOffH5 :: Element a => Ptr a -> Int -> IO a
{-# INLINE peekByteOffH5 #-}
peekByteOffH5 ptr off = peekH5 (ptr `plusPtr` off)

pokeByteOffH5 :: Element a => Ptr a -> Int -> a -> IO ()
{-# INLINE pokeByteOffH5 #-}
pokeByteOffH5 ptr off = pokeH5 (ptr `plusPtr` off)

allocaElement :: forall a b. Element a => (Ptr a -> IO b) -> IO b
allocaElement = allocaBytesAligned (fastSizeOfH5 @a) (alignmentH5 @a)

advancePtrH5 :: forall a. Element a => Ptr a -> Int -> Ptr a
{-# INLINE advancePtrH5 #-}
advancePtrH5 ptr off = ptr `plusPtr` (off * fastSizeOfH5 @a)

mallocVectorH5 :: forall a. Element a => Int -> IO (ForeignPtr a)
{-# INLINE mallocVectorH5 #-}
mallocVectorH5 size = mallocPlainForeignPtrAlignedBytes
  (size * fastSizeOfH5 @a)
  (alignmentH5 @a)


instance Element Int where
  typeH5 | wordSizeInBits == 64 = pure tyI64
         | otherwise            = pure tyI32
  fastSizeOfH5 = sizeOf    (undefined :: Int)
  alignmentH5  = alignment (undefined :: Int)
  peekH5       = peek
  pokeH5       = poke
instance Element Word where
  typeH5 | wordSizeInBits == 64 = pure tyU64
         | otherwise            = pure tyU32
  fastSizeOfH5 = sizeOf    (undefined :: Word)
  alignmentH5  = alignment (undefined :: Word)
  peekH5       = peek
  pokeH5       = poke


instance Element Int8   where
  typeH5       = pure tyI8
  fastSizeOfH5 = sizeOf    (undefined :: Int8)
  alignmentH5  = alignment (undefined :: Int8)
  peekH5       = peek
  pokeH5       = poke
instance Element Int16  where
  typeH5       = pure tyI16
  fastSizeOfH5 = sizeOf    (undefined :: Int16)
  alignmentH5  = alignment (undefined :: Int16)
  peekH5       = peek
  pokeH5       = poke
instance Element Int32  where
  typeH5       = pure tyI32
  fastSizeOfH5 = sizeOf    (undefined :: Int32)
  alignmentH5  = alignment (undefined :: Int32)
  peekH5       = peek
  pokeH5       = poke
instance Element Int64  where
  typeH5       = pure tyI64
  fastSizeOfH5 = sizeOf    (undefined :: Int64)
  alignmentH5  = alignment (undefined :: Int64)
  peekH5       = peek
  pokeH5       = poke
instance Element Word8  where
  typeH5       = pure tyU8
  fastSizeOfH5 = sizeOf    (undefined :: Word8)
  alignmentH5  = alignment (undefined :: Word8)
  peekH5       = peek
  pokeH5       = poke
instance Element Word16 where
  typeH5       = pure tyU16
  fastSizeOfH5 = sizeOf    (undefined :: Word16)
  alignmentH5  = alignment (undefined :: Word16)
  peekH5       = peek
  pokeH5       = poke
instance Element Word32 where
  typeH5       = pure tyU32
  fastSizeOfH5 = sizeOf    (undefined :: Word32)
  alignmentH5  = alignment (undefined :: Word32)
  peekH5       = peek
  pokeH5       = poke
instance Element Word64 where
  typeH5       = pure tyU64
  fastSizeOfH5 = sizeOf    (undefined :: Word64)
  alignmentH5  = alignment (undefined :: Word64)
  peekH5       = peek
  pokeH5       = poke

instance Element Float  where
  typeH5       = pure tyF32
  fastSizeOfH5 = sizeOf    (undefined :: Float)
  alignmentH5  = alignment (undefined :: Float)
  peekH5       = peek
  pokeH5       = poke
instance Element Double where
  typeH5       = pure tyF64
  fastSizeOfH5 = sizeOf    (undefined :: Double)
  alignmentH5  = alignment (undefined :: Double)
  peekH5       = peek
  pokeH5       = poke

-- | Uses same convention as @h5py@ by default.
instance Element a => Element (Complex a) where
  typeH5       = do ty <- typeH5 @a
                    makePackedRecord [("r",ty), ("i",ty)]
  fastSizeOfH5 = 2 * fastSizeOfH5 @a
  alignmentH5  = alignmentH5 @a
  peekH5 ptr   = (:+) <$> peekH5 (castPtr ptr) <*> peekElemOffH5 (castPtr ptr) 1
  pokeH5 ptr (re :+ im) = do
    pokeH5        (castPtr ptr)   re
    pokeElemOffH5 (castPtr ptr) 1 im


instance (Element a, FM.Prod a v) => Element (FM.ViaFixed a v) where
  typeH5       = do ty <- typeH5 @a
                    makeArray ty [FM.length (undefined :: FM.ViaFixed a v)]
  fastSizeOfH5 = fastSizeOfH5 @a *  FM.length (undefined :: FM.ViaFixed a v)
  alignmentH5  = alignmentH5  @a
  peekH5 ptr   = FM.generateM (peekElemOffH5 (castPtr ptr))
  pokeH5 ptr v = FM.imapM_ (pokeElemOffH5 (castPtr ptr)) v
  {-# INLINE fastSizeOfH5 #-}
  {-# INLINE alignmentH5  #-}
  {-# INLINE peekH5       #-}
  {-# INLINE pokeH5       #-}

instance (Element a, F.Vector v a) => Element (F.ViaFixed v a) where
  typeH5       = do ty <- typeH5 @a
                    makeArray ty [FM.length (undefined :: F.ViaFixed v a)]
  fastSizeOfH5 = fastSizeOfH5 @a *  F.length (undefined :: F.ViaFixed v a)
  alignmentH5  = alignmentH5  @a
  peekH5 ptr   = F.generateM (peekElemOffH5 (castPtr ptr))
  pokeH5 ptr v = F.imapM_ (pokeElemOffH5 (castPtr ptr)) v
  {-# INLINE fastSizeOfH5 #-}
  {-# INLINE alignmentH5  #-}
  {-# INLINE peekH5       #-}
  {-# INLINE pokeH5       #-}

deriving via (F.ViaFixed (FB.Vec n) a)
    instance (Element a, F.Arity n) => Element (FB.Vec n a)
deriving via (F.ViaFixed (FV.Vec n) a)
    instance (Element a, F.Arity n) => Element (FV.Vec n a)
deriving via (F.ViaFixed (FU.Vec n) a)
    instance (Element a, F.Arity n, FU.Unbox n a) => Element (FU.Vec n a)
deriving via (F.ViaFixed (FS.Vec n) a)
    instance (Element a, F.Arity n, Storable a) => Element (FS.Vec n a)
deriving via (F.ViaFixed (FP.Vec n) a)
    instance (Element a, F.Arity n, FP.Prim a) => Element (FP.Vec n a)

deriving newtype instance Element a => Element (Identity a)

wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)


#if MIN_VERSION_base(4,17,0)
-- | Derives HDF5 type as record. Uses 'makePackedRecord'
instance ( Generic a
         , GRecElement (Rep a)
         ) => Element (Generically a) where
  typeH5       = makePackedRecord =<< gtypeH5 @(Rep a)
  alignmentH5  = galignmentH5 @(Rep a)
  fastSizeOfH5 = gfastSizeOfH5 @(Rep a)
  peekH5 p = do
    (f,_) <- gpeekH5 (castPtr p) 0
    pure $! Generically $ to f
  pokeH5 p (Generically a) = void $ gpokeH5 (castPtr p) 0 (from a)
  {-# INLINE typeH5       #-}
  {-# INLINE alignmentH5  #-}
  {-# INLINE fastSizeOfH5 #-}
  {-# INLINE peekH5       #-}
  {-# INLINE pokeH5       #-}

class GRecElement f where
  gtypeH5 :: Hdf5M s [(String,Type)]
  gfastSizeOfH5 :: Int
  galignmentH5 :: Int
  gpeekH5 :: Ptr () -> Int -> IO (f p, Int)
  gpokeH5 :: Ptr () -> Int -> f p -> IO Int

deriving newtype instance GRecElement f => GRecElement (M1 D i f)
deriving newtype instance GRecElement f => GRecElement (M1 C i f)

instance (GRecElement f, GRecElement g) => GRecElement (f :*: g) where
  gtypeH5       = liftA2 (<>) (gtypeH5 @f) (gtypeH5 @g)
  gfastSizeOfH5 = gfastSizeOfH5 @f + gfastSizeOfH5 @g
  galignmentH5  = galignmentH5 @f `max` galignmentH5 @g
  gpeekH5 p i = do (f, i')  <- gpeekH5 p i
                   (g, i'') <- gpeekH5 p i'
                   pure (f :*: g, i'')
  gpokeH5 p i (f :*: g) = do i' <- gpokeH5 p i f
                             gpokeH5 p i' g
  {-# INLINE gtypeH5       #-}
  {-# INLINE gfastSizeOfH5 #-}
  {-# INLINE galignmentH5  #-}
  {-# INLINE gpeekH5       #-}
  {-# INLINE gpokeH5       #-}

instance ( KnownSymbol fld
         , Element a
         ) => GRecElement (M1 S (MetaSel (Just fld) u s l) (K1 r a)) where
  gtypeH5 = do ty <- typeH5 @a
               pure [(symbolVal (Proxy @fld), ty)]
  gfastSizeOfH5 = fastSizeOfH5 @a
  galignmentH5  = alignmentH5  @a
  gpeekH5 p i = do
    a <- peekH5 (castPtr $ plusPtr p i)
    pure (M1 (K1 a), i+len)
    where
      len = fastSizeOfH5 @a
  gpokeH5 p i (M1 (K1 a)) = do
    pokeH5 (castPtr $ plusPtr p i) a
    pure $! i + len
    where
      len = fastSizeOfH5 @a
  {-# INLINE gtypeH5       #-}
  {-# INLINE gfastSizeOfH5 #-}
  {-# INLINE galignmentH5  #-}
  {-# INLINE gpeekH5       #-}
  {-# INLINE gpokeH5       #-}
#endif
