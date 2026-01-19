{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- API for working with attributes of datasets\/groups.
module HDF5.HL.Attribute
{-  ( -- * Attributes
    Attribute
  , openAttrMay
  , withAttrMay
  , readAttrMay
  , writeAttr
    -- * Type class
  , SerializeAttr(..)
    -- ** Writing attributes
  , AttributeWriter
  , runAttributeWriter
  , encodeAttr
    -- ** Parsing of attributes
  , AttributeParser
  , runAttributeParserEither
  , runAttributeParser
  -- , AttributeM
  -- , ReadAttr
  -- , WriteAttr
  -- , runAttributeM
  -- , encodeAttr
  -- , decodeAttrMay
  -- , decodeAttr
  -- , attrSubset
  ) -} where

import Control.Applicative
-- import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.List                 (intercalate)
import Data.List.NonEmpty        qualified as NE
import Foreign.C.String
import Foreign.Marshal
import GHC.Stack

import HDF5.HL.Unsafe.Types
import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Unsafe.Error
import HDF5.HL.Serialize
import HDF5.HL.Dataspace
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
writeAttr d name a = withFrozenCallStack $ evalContT $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString name
  space  <- ContT $ withCreateDataspaceFromExtent (getExtent a)
  tid    <- ContT $ withType $ typeH5 @(ElementOf a)
  attr   <- ContT $ bracket
    ( fmap Attribute
    $ checkHID p_err ("Cannot create attribute " ++ name)
    $ h5a_create (getHID d) c_path tid (getHID space)
          H5P_DEFAULT
          H5P_DEFAULT)
    basicClose
  lift $ writeAll attr a


----------------------------------------------------------------
-- Typeclass based API
----------------------------------------------------------------

data AttrTy = IsAttr
            | IsAttrValue

class SerializeAttr a where
  type AttrType a :: AttrTy
  attrParser :: AttributeParser (AttrType a) a
  attrWriter :: a -> AttributeWriter (AttrType a)


-- | Writer for attributes which is used to simulate nested namespace
--   of attributes.
newtype AttributeWriter (t :: AttrTy) = AttributeWriter
  { unAttributeWriter :: forall d. HasAttrs d => d -> PathTransform t -> IO ()
  }

instance Semigroup (AttributeWriter t) where
  AttributeWriter f <> AttributeWriter g = AttributeWriter $ \d mk_name ->
    f d mk_name <> g d mk_name
instance Monoid (AttributeWriter t) where
  mempty =  AttributeWriter $ \_ _ -> pure ()



primValueWriter :: ArrayLike a => a -> AttributeWriter IsAttrValue
primValueWriter a = AttributeWriter $ \d mk_name -> do
  let name = case mk_name of
        TransformLeaf fun -> intercalate "/" $ NE.toList $ fun []
  writeAttr d name a

writeAttrAt :: FilePath -> (a -> AttributeWriter t) -> (a -> AttributeWriter IsAttr)
writeAttrAt nm writ a = AttributeWriter $ \d mk_name ->
  case writ a of
    AttributeWriter fun -> fun d $ case mk_name of
      TransformAttr f -> TransformLeaf ((nm NE.:|) . f)
      TransformLeaf f -> TransformLeaf (NE.cons nm . f)



-- | Evaluate attribute writer
runAttributeWriter :: HasAttrs d => d -> AttributeWriter IsAttr -> IO ()
runAttributeWriter d f = unAttributeWriter f d (TransformAttr id)



-- | Parser for decoding of attributes
newtype AttributeParser (t :: AttrTy) a = AttributeParser
  { unAttributeParser :: forall r d. HasAttrs d
                      => d
                      -> PathTransform t
                      -> (AttributeParseError -> IO r)
                      -> (a                   -> IO r)
                      -> IO r
  }
  deriving stock Functor

data PathTransform ty where
  TransformLeaf :: ([FilePath] -> NE.NonEmpty FilePath) -> PathTransform t
  TransformAttr :: ([FilePath] -> [FilePath])           -> PathTransform IsAttr

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
  = parser d (TransformAttr id) (pure . Left) (pure . Right)

-- | Run parser for attributes. Parser errors would be thrown as exceptions
runAttributeParser
  :: (HasAttrs d)
  => d
  -> AttributeParser IsAttr a
  -> IO a
runAttributeParser d (AttributeParser parser)
  = parser d (TransformAttr id) throwM pure

primValueParser :: ArrayLike a => AttributeParser IsAttrValue a
primValueParser = AttributeParser $ \d mk_name c_fail c_succ -> do
  let name = case mk_name of
        TransformLeaf fun -> intercalate "/" $ NE.toList $ fun []
  readAttrMay d name >>= \case
    Nothing -> c_fail $ MissingAttribute name
    Just a  -> c_succ a

parseAtPath :: FilePath -> AttributeParser t a -> AttributeParser IsAttr a
parseAtPath nm (AttributeParser parser) = AttributeParser $ \d mk_name ->
  parser d (case mk_name of
              TransformAttr f -> TransformLeaf ((nm NE.:|) . f)
              TransformLeaf f -> TransformLeaf (NE.cons nm . f)
           )



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance SerializeAttr () where
  type AttrType () = IsAttr
  attrParser   = pure ()
  attrWriter _ = mempty

instance SerializeAttr Int where
  type AttrType Int = IsAttrValue
  attrParser = primValueParser
  attrWriter = primValueWriter

instance SerializeAttr a => SerializeAttr (Maybe a) where
  type AttrType (Maybe a) = AttrType a
  attrWriter Nothing  = mempty
  attrWriter (Just a) = attrWriter a
  attrParser = optional attrParser
