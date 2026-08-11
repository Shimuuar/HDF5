{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- API for working with attributes of datasets\/groups.
module HDF5.HL.Attribute
  ( -- * Attributes
    Attribute
  , openAttrMay
  , withAttrMay
  , readAttrMay
  , writeAttr
    -- * Type class API
  , SerializeAttr(..)
  , writeAttributes
  , readAttributes
  , readAttributesEither
  , AttrTy(..)
    -- ** Deriving
  , AsAttributeValue(..)
    -- ** Attribute writer
  , AttributeWriter
  , runAttributeWriter
  , writeAttrValue
  , writeAttrSet
    -- ** Attribute parser
  , AttributeParser
  , runAttributeParserEither
  , runAttributeParser
  , parseAttrValue
  , parseAttrSet
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Int
import Data.Word
import Data.Coerce
import Data.List                   (intercalate)
import Data.List.NonEmpty          qualified as NE
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Storable  qualified as FS
import Data.Vector.Fixed.Primitive qualified as FP
import Data.Vector.Fixed.Strict    qualified as FV
import Data.Vector                 qualified as V
import Data.Vector.Storable        qualified as VS
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Strict          qualified as VV
import Data.Vector.Primitive       qualified as VP
import Data.Proxy
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal
import GHC.Stack
import GHC.Generics
import GHC.TypeLits

import HDF5.HL.Unsafe.Types
import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Unsafe.Error
import HDF5.HL.Serialize
import HDF5.HL.Dataspace
import HDF5.HL.Vector
import HDF5.HL.Monad
import HDF5.C

----------------------------------------------------------------
-- Simple API
----------------------------------------------------------------

-- | Open attribute on group or dataset. Returns nothing if dataset
--   does not exists.
openAttrMay
  :: (HasAttrs d, MonadIO m, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> m (Maybe Attribute)
openAttrMay (getHID -> hid) path = liftIO $ withFrozenCallStack $ evalContT $ do
  p_err <- ContT $ alloca
  c_str <- ContT $ withCString path
  lift $ do
    exists <- checkHTri p_err ("Cannot check whether attribute " ++ path ++ " exists")
            $ h5a_exists hid c_str
    case exists of
      False -> pure Nothing
      True  -> Just . Attribute
            <$> ( checkHID p_err ("Cannot open attribute " ++ path)
                $ h5a_open hid c_str H5P_DEFAULT)

-- | Bracket variant of 'openAttrMay'.
withAttrMay
  :: (HasAttrs d, MonadIO m, MonadMask m, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> (Maybe Attribute -> m b)
  -> m b
withAttrMay a path = bracket (openAttrMay a path) (mapM_ (liftIO . basicClose))


-- | Read attribute from. Return @Nothing@ if attribute doesn't
--   exists, and throws exception if it couldn't be decoded.
readAttrMay
  :: (ArrayLike a, HasAttrs d, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> IO (Maybe a)
readAttrMay d name = withAttrMay d name $ \case
  Just x  -> Just <$> readAll x
  Nothing -> pure Nothing

-- | Create attribute.
writeAttr
  :: forall a d. (ArrayLike a, HasAttrs d, HasCallStack)
  => d      -- ^ Dataset or group
  -> String -- ^ Attribute name
  -> a      -- ^ Value to write
  -> IO ()
writeAttr d name a = withFrozenCallStack $ runHdf5M $ do
  c_path <- liftBracket $ withCString name
  space  <- hdfCreateDataspaceFromExtent (getExtent a)
  ty_a   <- typeH5 @(ElementOf a)
  attr   <- boundCheckHID ("Cannot create attribute " ++ name) Attribute
          $ h5a_create (getHID d) c_path (getTypeHID ty_a) (getHID space)
                H5P_DEFAULT
                H5P_DEFAULT
  liftIO $ writeAll attr a


----------------------------------------------------------------
-- Typeclass based API
----------------------------------------------------------------


-- | Type class for reading and writing haskell values as set of HDF5
--   attributes. Here we have to solve several problems. Attributes
--   live in flat namespace and in order to serialize records we need
--   hierarchical one. We simulate hierarchy by using directory-like
--   names.
--
--   > foo/a1
--   > foo/a2
--   > bar
--
--   This class must be derivable. And thus we need to be able write
--   instances for types representing attribute values: @Int@,
--   @Vector@, etc. They aren't attributes proper they don't have name
class SerializeAttr a where
  type AttrType a :: AttrTy
  attrParser :: AttributeParser (AttrType a) a
  attrWriter :: a -> AttributeWriter (AttrType a)

-- | Write haskell value as HDF5 attributes.
writeAttributes
  :: (SerializeAttr a, HasAttrs d, MonadIO m, AttrType a ~ IsAttr)
  => d -- ^ HDF5 object
  -> a -- ^ Value to serialize
  -> m ()
writeAttributes d a
  = liftIO $ runAttributeWriter d (attrWriter a)

-- | Read haskell value from HDF5 attributes.
readAttributes
  :: (SerializeAttr a, HasAttrs d, MonadIO m, AttrType a ~ IsAttr)
  => d -- ^ HDF5 object
  -> m a
readAttributes d
  = liftIO $ runAttributeParser d attrParser

-- | Read haskell value from HDF5 attributes.
readAttributesEither
  :: (SerializeAttr a, HasAttrs d, MonadIO m, AttrType a ~ IsAttr)
  => d -- ^ HDF5 object
  -> m (Either AttributeParseError a)
readAttributesEither d
  = liftIO $ runAttributeParserEither d attrParser


-- | Type tag for testing whether parser\/writer could be used or it
--   merely describes how attribute values should be parsed if
--   attribute name is provided
data AttrTy
  = IsAttr      -- ^ This is proper attribute
  | IsAttrValue -- ^ This is only attribute value

data PathTransform ty where
  TransformLeaf :: ([FilePath] -> NE.NonEmpty FilePath) -> PathTransform t
  TransformAttr :: PathTransform IsAttr


----------------------------------------------------------------
-- Writer

type role AttributeWriter nominal

-- | Writer for attributes which is used to simulate nested namespace
--   of attributes.
newtype AttributeWriter (t :: AttrTy) = AttributeWriter
  { unAttributeWriter :: forall d. HasAttrs d => d -> PathTransform t -> IO ()
  }

-- | Evaluate attribute writer
runAttributeWriter
  :: HasAttrs d
  => d                      -- ^ HDF5 object to add attributes to
  -> AttributeWriter IsAttr
  -> IO ()
runAttributeWriter d f = unAttributeWriter f d TransformAttr


instance Semigroup (AttributeWriter t) where
  AttributeWriter f <> AttributeWriter g = AttributeWriter $ \d mk_name ->
    f d mk_name <> g d mk_name

instance Monoid (AttributeWriter t) where
  mempty = AttributeWriter $ \_ _ -> pure ()


-- | Writer for attribute value. It couldn't be executed and could
--   only be used as part of other parsers. See 'attributeSet'
writeAttrValue :: ArrayLike a => a -> AttributeWriter IsAttrValue
writeAttrValue a = AttributeWriter $ \d mk_name -> do
  let name = case mk_name of
        TransformLeaf fun -> intercalate "/" $ NE.toList $ fun []
  writeAttr d name a


-- | Prepend string to names of all attributes.
writeAttrSet
  :: FilePath
  -- ^ Name to prepend
  -> (a -> AttributeWriter t)
  -- ^ Attribute writer.
  -> (a -> AttributeWriter IsAttr)
writeAttrSet nm writ a = AttributeWriter $ \d mk_name ->
  case writ a of
    AttributeWriter fun -> fun d $ case mk_name of
      TransformAttr   -> TransformLeaf (nm NE.:|)
      TransformLeaf f -> TransformLeaf (f . (nm:))


----------------------------------------------------------------
-- Parser

type role AttributeParser nominal representational

-- | Parser for decoding of attributes. It backtracks on missing
--   attributes but reading errors results in exception.
newtype AttributeParser (t :: AttrTy) a = AttributeParser
  { unAttributeParser :: forall r d. HasAttrs d
                      => d
                      -> PathTransform t
                      -> (AttributeParseError -> IO r)
                      -> (a                   -> IO r)
                      -> IO r
  }
  deriving stock Functor

instance Applicative (AttributeParser t) where
  pure a = AttributeParser $ \_ _ _ c_succ -> c_succ a
  AttributeParser pF <*> AttributeParser pA
    = AttributeParser
    $ \d p c_fail c_succ -> pF d p c_fail
    $ \f -> pA d p c_fail (c_succ . f)

instance Alternative (AttributeParser t) where
  empty = AttributeParser $ \_ _ c_fail _ -> c_fail (AttributeParseError "Alternative.empty")
  AttributeParser pA <|> AttributeParser pB
    = AttributeParser
    $ \d p c_fail c_succ -> pA d p (\_ -> pB d p c_fail c_succ) c_succ

instance Monad (AttributeParser t) where
  m >>= f
    = AttributeParser
    $ \d p c_fail c_succ -> unAttributeParser m d p c_fail
    $ \a -> unAttributeParser (f a) d p c_fail c_succ

instance MonadFail (AttributeParser t) where
  fail e = AttributeParser $ \_ _ c_fail _ -> c_fail (AttributeParseError $ "fail: " ++ e)

-- | Run parser for attributes
runAttributeParserEither
  :: (HasAttrs d)
  => d
  -> AttributeParser IsAttr a
  -> IO (Either AttributeParseError a)
runAttributeParserEither d (AttributeParser parser)
  = parser d TransformAttr (pure . Left) (pure . Right)

-- | Run parser for attributes. Parser errors would be thrown as exceptions
runAttributeParser
  :: (HasAttrs d)
  => d
  -> AttributeParser IsAttr a
  -> IO a
runAttributeParser d (AttributeParser parser)
  = parser d TransformAttr throwM pure


-- | Parser for attribute value.
parseAttrValue :: ArrayLike a => AttributeParser IsAttrValue a
parseAttrValue = AttributeParser $ \d mk_name c_fail c_succ -> do
  let name = case mk_name of
        TransformLeaf fun -> intercalate "/" $ NE.toList $ fun []
  readAttrMay d name >>= \case
    Nothing -> c_fail $ MissingAttribute name
    Just a  -> c_succ a

parseAttrSet :: FilePath -> AttributeParser t a -> AttributeParser IsAttr a
parseAttrSet nm (AttributeParser parser) = AttributeParser $ \d mk_name ->
  parser d (case mk_name of
              TransformAttr   -> TransformLeaf (nm NE.:|)
              TransformLeaf f -> TransformLeaf (f . (nm:))
           )



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

newtype AsAttributeValue a = AsAttributeValue a

instance ArrayLike a => SerializeAttr (AsAttributeValue a) where
  type AttrType (AsAttributeValue a) = IsAttrValue
  attrParser = coerce (parseAttrValue @a)
  attrWriter = coerce (writeAttrValue @a)


instance (Generic a, GSerializeAttr (Rep a)) => SerializeAttr (Generically a) where
  type AttrType (Generically a) = IsAttr
  attrParser = Generically . to <$> gattrParser
  attrWriter (Generically a) = gattrWriter $ from a

class GSerializeAttr f where
  gattrParser :: AttributeParser IsAttr (f p)
  gattrWriter :: f p -> AttributeWriter IsAttr

deriving newtype instance GSerializeAttr f => GSerializeAttr (M1 D c f)
deriving newtype instance GSerializeAttr f => GSerializeAttr (M1 C c f)
instance (GSerializeAttr f, GSerializeAttr g) => GSerializeAttr (f :*: g) where
  gattrParser = liftA2 (:*:) gattrParser gattrParser
  gattrWriter (f :*: g) = gattrWriter f <> gattrWriter g

instance ( KnownSymbol field, SerializeAttr a
         ) => GSerializeAttr (M1 S (MetaSel (Just field) u ss ds) (K1 i a)) where
  gattrParser = parseAttrSet (symbolVal (Proxy @field))
              $ coerce (attrParser @a)
  gattrWriter (M1 (K1 a)) = writeAttrSet (symbolVal (Proxy @field)) attrWriter a



-- | Doesn't read or write attributes.
instance SerializeAttr () where
  type AttrType () = IsAttr
  attrParser   = pure ()
  attrWriter _ = mempty

instance SerializeAttr a => SerializeAttr (Maybe a) where
  type AttrType (Maybe a) = AttrType a
  attrWriter Nothing  = mempty
  attrWriter (Just a) = attrWriter a
  attrParser = optional attrParser


deriving via AsAttributeValue Int    instance SerializeAttr Int
deriving via AsAttributeValue Int8   instance SerializeAttr Int8
deriving via AsAttributeValue Int16  instance SerializeAttr Int16
deriving via AsAttributeValue Int32  instance SerializeAttr Int32
deriving via AsAttributeValue Int64  instance SerializeAttr Int64
deriving via AsAttributeValue Word   instance SerializeAttr Word
deriving via AsAttributeValue Word8  instance SerializeAttr Word8
deriving via AsAttributeValue Word16 instance SerializeAttr Word16
deriving via AsAttributeValue Word32 instance SerializeAttr Word32
deriving via AsAttributeValue Word64 instance SerializeAttr Word64
deriving via AsAttributeValue Float  instance SerializeAttr Float
deriving via AsAttributeValue Double instance SerializeAttr Double

deriving via AsAttributeValue (FB.Vec n a)
    instance (F.Arity n, Element a) => SerializeAttr (FB.Vec n a)
deriving via AsAttributeValue (FU.Vec n a)
    instance (F.Arity n, Element a, FU.Unbox n a) => SerializeAttr (FU.Vec n a)
deriving via AsAttributeValue (FS.Vec n a)
    instance (F.Arity n, Element a, Storable a) => SerializeAttr (FS.Vec n a)
deriving via AsAttributeValue (FP.Vec n a)
    instance (F.Arity n, Element a, FP.Prim a) => SerializeAttr (FP.Vec n a)
deriving via AsAttributeValue (FV.Vec n a)
    instance (F.Arity n, Element a) => SerializeAttr (FV.Vec n a)

deriving via AsAttributeValue [a] instance Element a => SerializeAttr [a]
deriving via AsAttributeValue (VecHDF5 a)
    instance (Element a) => SerializeAttr (VecHDF5 a)
deriving via AsAttributeValue (V.Vector a)
    instance (Element a) => SerializeAttr (V.Vector a)
deriving via AsAttributeValue (VV.Vector a)
    instance (Element a) => SerializeAttr (VV.Vector a)
deriving via AsAttributeValue (VS.Vector a)
    instance (Element a, VS.Storable a) => SerializeAttr (VS.Vector a)
deriving via AsAttributeValue (VP.Vector a)
    instance (Element a, VP.Prim a) => SerializeAttr (VP.Vector a)
deriving via AsAttributeValue (VU.Vector a)
    instance (Element a, VU.Unbox a) => SerializeAttr (VU.Vector a)
