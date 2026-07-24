{-|

This library provides high level API for working with HDF5 files. HDF5
stands for [Hierarchical Data Format
v5](https://www.hdfgroup.org/solutions/hdf5/) and geared for working
with large scale array data. API leans very heavily into type classes.


= Overview

Each HDF5 file is organized as hierarchical namespace (file system
like) and contains named 'Dataset's and 'Group's (directories) which
could contain @Group@s and @Dataset@s. In turn @Dataset@ is dense
N-dimensional (up to 32) array of elements of some type. HDF5 supports
wide range of types: primitives like fixed width ints, IEEE754
floating points, records, enumerations. @Dataset@s could be read and
written to using slices. Currently support for that is only partial.
Both @Group@s and @Dataset@s support 'Attribute's, named values:
scalars or small arrays.


== HDF5 entities

HDF5 define number of entities with lifetimes that should be managed
by programmer:

- 'File' — handle to HDF5 file. It also could be used as root group.

- 'Group' — handle to group (directory like). Note that in places which
  expect group @File@ represents root directory of a file. (See 'IsDirectory')

- 'Dataset' — handle to dataset in the file. It could be used to both
  read from and write to dataset.

- 'Attribute' — named values which could be attached to group or
  dataset. API for working with them is defined in "HDF5.HL.Attribute" module.

- 'Dataspace' — it encodes dimensions of dataset. Usually one doesn't
   need to deal with it directly and API uses haskell values for size
   of array. See 'IsExtent' and 'IsDataspace' for details.

All of them could be closed in the same way by calling
'close'. Functions that open\/create such entities have bracket-like
companion named @with...@


== Reading/writing of datasets

Datasets are dense N-dimensional arrays of some possibly complicated
type described by 'Type'. They could be stored as single contiguous
chunk or as set of chunks managed by library. Compression is also
supported. All these properties (see 'Property') are specified at the
time of dataset creation and completely transparent for
reading\/writing API.

Following type classes are used for IO:

- `ArrayLike` is for types which could be directly mapped to arrays
  and scalars (0-dimensional arrays) and is used for datasets and
  attributes alike.

- `SerializeDSet` is for values that could be serialized as dataset.
  It allows to use attributes in serialization as well.

- `HDF5.HL.SerializeH5.SerializeH5` type class which allows to
  serialize haskell values as arbitrary trees of HDF values.
-}
module HDF5.HL
  ( -- * Files and groups
    -- ** File operations
    File
  , OpenMode(..)
  , CreateMode(..)
  , openFile
  , withOpenFile
  , createFile
  , withCreateFile
    -- ** Group operation
  , Group
  , openGroup
  , createGroup
  , withOpenGroup
  , withCreateGroup
  , listGroup
  , delete
  , pathIsValid
    -- * Datasets
  , Dataset
  , rank
  , extent
    -- ** Opening & creation
  , openDataset
  , createEmptyDataset
  , withOpenDataset
  , withCreateEmptyDataset
  , setDatasetExtent
    -- ** Reading & writing of arrays
  , writeSlab
  , readSlab
  , writeAll
  , readAll
  , writeAllAt
  , readAllAt
  , ArrayLike(..)
    -- ** Reading using 'SerializeDSet'
  , readDatasetAt
  , writeDatasetAt
  , SerializeDSet(..)
    -- **  Deriving via
  , SerializeAsScalar(..)
  , SerializeAsArray(..)
    -- * Dataspace information
    -- $dataspace
  , IsExtent(..)
  , IsDataspace(..)
  , pattern UNLIMITED
  , Extent(..)
  , Growable(..)
    -- * Data types
    -- $type_hdf
  , Type
  , sizeOfH5
  , Element(..)
    -- * Property lists
  , Property
  , Layout(..)
  , propDatasetLayout
  , propDatasetChunking
  , propDatasetDeflate
    -- * Type classes
  , IsObject
  , IsDirectory
  , HasData(..)
  , getType
  , HasAttrs
  , Closable
  , close
    -- * Error handling
  , Error(..)
  , MajError(..)
  , MinError(..)
  , Message(..)
  , DataspaceParseError(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

import HDF5.HL.Serialize
import HDF5.HL.Unsafe.Types
import HDF5.HL.Unsafe.Wrappers
import HDF5.HL.Unsafe.Error
import HDF5.HL.Unsafe.Enum
import HDF5.HL.Dataspace
import HDF5.HL.Unsafe.Property
import HDF5.HL.Unsafe.Encoding
import HDF5.HL.Attribute
import HDF5.C
import Prelude hiding (read,readIO)


-- $type_hdf
--
-- See module "HDF5.HL.Types" for full API for defining and matching
-- HDF5 types.

----------------------------------------------------------------
-- Lifted function from other modules
----------------------------------------------------------------

-- | Close value
close :: (Closable a, MonadIO m, HasCallStack) => a -> m ()
close = liftIO . basicClose

getType :: (HasData a, MonadIO m, HasCallStack) => a -> m Type
getType = liftIO . getTypeIO

----------------------------------------------------------------
-- File API
----------------------------------------------------------------

-- | Open HDF5 file. Use 'createFile' to create new file. Returned
--   handle must be closed using 'close'.
openFile :: (MonadIO m, MonadThrow m, HasCallStack) => FilePath -> OpenMode -> m File
openFile path = \case
  OpenRO     -> native h5f_ACC_RDONLY >>= either throwM pure
  OpenRW     -> native h5f_ACC_RDWR   >>= either throwM pure
  OpenAppend -> native h5f_ACC_RDWR   >>= \case
    Right a -> pure a
    Left (Error _ (Message{msgMajorN=MAJ_FILE,msgMinorN=MIN_CANTOPENFILE}:_))
           -> createFile path CreateExcl
    Left e -> throwM e
  where
    native mode = propagateEither $ do
      p_err  <- ContT $ alloca
      c_path <- ContT $ withCString path
      fmap File
        $ contCheckHID p_err ("Cannot open file " ++ path)
        $ h5f_open c_path mode H5P_DEFAULT

-- | Open file using 'openFile' and pass handle to continuation. It
--   will be closed when continuation finish execution normally or
--   abnormally.
withOpenFile
  :: (MonadMask m, MonadIO m, HasCallStack)
  => FilePath -> OpenMode -> (File -> m a) -> m a
withOpenFile path mode = bracket (openFile path mode) close

-- | Create new HDF5 file or replace existing file. Use 'openFile' to
--   open existing file for modification. Returned handle must be
--   closed using 'close'.
createFile :: (MonadIO m, MonadThrow m, HasCallStack) => FilePath -> CreateMode -> m File
createFile path mode = propagateError $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  fmap File
    $ contCheckHID p_err ("Cannot create file " ++ path)
    $ h5f_create c_path (toCParam mode) H5P_DEFAULT H5P_DEFAULT

-- | Create file using 'createFile' and pass handle to
--   continuation. It will be closed when continuation finish
--   execution normally or abnormally.
withCreateFile
  :: (MonadMask m, MonadIO m, HasCallStack)
  => FilePath -> CreateMode -> (File -> m a) -> m a
withCreateFile path mode = bracket (createFile path mode) close

----------------------------------------------------------------
-- Group API
----------------------------------------------------------------

-- | Obtain handle to directory 
openGroup
  :: (IsDirectory dir, MonadIO m, MonadThrow m, HasCallStack)
  => dir      -- ^ Location. Either 'File' or 'Group'
  -> FilePath -- ^ Name of group
  -> m Group
openGroup dir path = propagateError $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  fmap Group
    $ contCheckHID p_err ("Cannot open group " ++ path)
    $ h5g_open (getHID dir) c_path H5P_DEFAULT

-- | @bracket@-style wrapper for 'openGroup'
withOpenGroup
  :: (IsDirectory dir, MonadIO m, MonadMask m, HasCallStack)
  => dir              -- ^ Location. Either 'File' or 'Group'
  -> FilePath         -- ^ Name of group
  -> (Group -> m a)
  -> m a
withOpenGroup dir path = bracket (openGroup dir path) close

-- | Create group in HDF5 file or in some group.
createGroup
  :: (IsDirectory dir, MonadIO m, MonadThrow m, HasCallStack)
  => dir       -- ^ Location. Either 'File' or 'Group'
  -> FilePath  -- ^ Name of group
  -> m Group
createGroup dir path = propagateError $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  fmap Group
    $ contCheckHID p_err ("Cannot create group " ++ path)
    $ h5g_create (getHID dir) c_path H5P_DEFAULT H5P_DEFAULT H5P_DEFAULT

-- | @bracket@-style wrapper for 'createGroup'
withCreateGroup
  :: (IsDirectory dir, MonadIO m, MonadMask m, HasCallStack)
  => dir              -- ^ Location. Either 'File' or 'Group'
  -> FilePath         -- ^ Name of group
  -> (Group -> m a)
  -> m a
withCreateGroup dir path = bracket (createGroup dir path) close

-- | List all names in the group
listGroup
  :: (IsDirectory dir, MonadIO m, MonadThrow m, HasCallStack)
  => dir -- ^ Location to use
  -> m [FilePath]
listGroup dir = propagateError$ do
  p_err <- ContT $ alloca
  p_idx <- ContT $ alloca
  names <- lift  $ newIORef []
  let readNode _hid cname _p_node _p_userdata = do
        name <- peekCString cname
        modifyIORef' names (name:)
        pure $ HErr 0
  callback <- ContT $ bracket (makeH5LIterate2 readNode) freeHaskellFunPtr
  liftIO $ poke p_idx 0 -- We MUST set starting index
  contCheckHErr p_err "Unable to iterate over group"
    $ h5l_iterate (getHID dir) H5_INDEX_NAME H5_ITER_DEC p_idx callback nullPtr
  liftIO $ readIORef names

-- | Delete object from group.
delete
  :: (IsDirectory dir, MonadIO m, MonadThrow m, HasCallStack)
  => dir      -- ^ Location to use
  -> FilePath -- ^ Name to delete
  -> m ()
delete dir path = propagateError $ do
  p_err  <- ContT $ alloca
  c_name <- ContT $ withCString path
  contCheckHErr p_err ("Unable to delete path: " ++ path)
    $ h5l_delete (getHID dir) c_name H5P_DEFAULT

-- | Check whether path in HDF5 file is valid.
--
--   If @check@ is @False@ only existence of links in path will be
--   checked. But it could point non nonexistent object (think
--   dangling symlink). If @check@ is @True@ function will verify that
--   path points to existing object in file.
--
--   Path could be relative in which case path to provided object is
--   checked or absolute (starts from @/@). In latter case parameter
--   is only used to find file in which do lookup any group\/file
--   handle will work.
pathIsValid
  :: (IsDirectory dir, MonadIO m, MonadThrow m, HasCallStack)
  => dir      -- ^ Location to use
  -> FilePath -- ^ Path to check
  -> Bool     -- ^ @check@ Whether to check that object pointed to path exists.
  -> m Bool
pathIsValid dir path check = propagateError $ do
  p_err  <- ContT $ alloca
  c_name <- ContT $ withCString path
  contCheckHTri p_err "pathIsValid"
    $ h5lt_path_valid (getHID dir) c_name (if check then 1 else 0)


----------------------------------------------------------------
-- Dataset API
----------------------------------------------------------------

-- $dataspace
--
-- In HDF5 terminology 'Dataspace' is object that encode dimensions of
-- an dataset or attribute array. It's also used to select parts of an
-- array for reading\/writing.
--
--  * Null dataspace it corresponds to empty dataset
--  * Scalar dataspace which corresponds to dataset containing single value
--  * Simple dataspace which corresponds to N-dimensional array. Up to
--  32-dimensional arrays are supported
--
-- 'Dataspace' is part of public API but it's expected that usually
-- dataset operations do not need to deal with it explicitly. Instead
-- we encode data dimensions and offsets using haskell values: fixnums
-- and tuples mostly. Type class 'IsExtent' encodes N-dimensional
-- products of @Word64@ and could be used as size or
-- offset. 'IsDataspace' is used for specifying dataset's dataspace
-- when creating it and for querying.



-- | Open existing dataset in given location. Returned 'Dataset' must
--   be closed by call to 'close'.
openDataset
  :: (MonadIO m, MonadThrow m, IsDirectory dir, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> m Dataset
openDataset dir path = propagateError $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  fmap Dataset
    $ contCheckHID p_err ("Cannot open dataset " ++ path)
    $ h5d_open2 (getHID dir) c_path H5P_DEFAULT

-- | Create new dataset at given location without writing any data to
--   it. This is standard way of creating dataset when data will be
--   written incrementally. Otherwise 'writeAllAt' or 'writeDatasetAt'
--   could be more convenient. It possible to change dataset extent
--   (size) after creation using 'setDatasetExtent' if it uses chunked
--   layout. See 'propDatasetChunking' for details. Returned 'Dataset'
--   must be closed by call to 'close'.
createEmptyDataset
  :: (MonadIO m, MonadThrow m, IsDirectory dir, IsDataspace ext, HasCallStack)
  => dir                -- ^ Location
  -> FilePath           -- ^ Path relative to location
  -> Type               -- ^ Element type
  -> ext                -- ^ Extent of dataset. See 'IsDataspace' for details.
  -> [Property Dataset] -- ^ Dataset creation properties
  -> m Dataset
createEmptyDataset dir path ty ext props = propagateError $ do
  p_err  <- ContT $ alloca
  c_path <- ContT $ withCString path
  space  <- ContT $ withCreateDataspaceFromDSpace ext
  tid    <- ContT $ withType ty
  plist  <- ContT $ withDatasetProps $ mconcat props
  fmap Dataset
    $ contCheckHID p_err ("Unable to create dataset")
    $ h5d_create (getHID dir) c_path tid (getHID space)
      H5P_DEFAULT
      (getHID plist)
      H5P_DEFAULT


-- | Open dataset and pass handle to continuation. Dataset will be
--   closed when continuation finish execution normally or with an
--   exception.
withOpenDataset
  :: (MonadMask m, MonadIO m, IsDirectory dir, HasCallStack)
  => dir      -- ^ Location
  -> FilePath -- ^ Path relative to location
  -> (Dataset -> m a)
  -> m a
withOpenDataset dir path = bracket (openDataset dir path) close

-- | Create new dataset using 'createEmptyDataset'. Dataset will be
--   closed when continuation finish execution normally or with an
--   exception.
withCreateEmptyDataset
  :: (MonadIO m, MonadMask m, IsDirectory dir, IsDataspace ext, HasCallStack)
  => dir       -- ^ Location
  -> FilePath  -- ^ Path relative to location
  -> Type      -- ^ Element type
  -> ext       -- ^ Dataspace, that is size of dataset
  -> [Property Dataset] -- ^ Dataset creation properties
  -> (Dataset -> m a)
  -> m a
withCreateEmptyDataset dir path ty ext props = bracket
  (createEmptyDataset dir path ty ext props)
  close

-- | Open and read dataset from either file or group.
readAllAt
  :: (ArrayLike a, IsDirectory dir, MonadIO m, MonadMask m, HasCallStack)
  => dir      -- ^ Location in HDF5 file
  -> FilePath -- ^ Dataset name
  -> m a
readAllAt dir path = withOpenDataset dir path readAll

-- | Create dataset and write haskell array into it.
writeAllAt
  :: forall a dir m. (ArrayLike a, IsDirectory dir, MonadIO m, MonadMask m, HasCallStack)
  => dir                -- ^ Location in HDF5 file
  -> FilePath           -- ^ Name dataset to create
  -> [Property Dataset] -- ^ Dataset properties
  -> a                  -- ^ Value to write  
  -> m ()
writeAllAt dir path prop a
  = withCreateEmptyDataset dir path (typeH5 @(ElementOf a)) (getExtent a) prop
  $ \dset -> writeAll dset a

-- | Read dataset from HDF5 using 'SerializeDSet' machinery.
readDatasetAt
  :: (SerializeDSet a, IsDirectory dir, MonadIO m, HasCallStack)
  => dir      -- ^ Location in HDF5 file
  -> FilePath -- ^ Path to dataset
  -> m a
readDatasetAt dir path
  = liftIO
  $ withOpenDataset dir path basicReadDSet

-- | Create dataset from haskell value of type @a@. 
writeDatasetAt
  :: forall a dir m. (SerializeDSet a, IsDirectory dir, MonadIO m, HasCallStack)
  => dir      -- ^ Location in HDF5 file
  -> FilePath -- ^ Path to dataset to create
  -> a        -- ^ Value to write to HDF5
  -> m ()
writeDatasetAt dir path a
  = liftIO
  $ basicWriteDSet a
  $ \ext ty prop action -> 
    withCreateEmptyDataset dir path ty ext prop action


-- | Set new extent of dataspace. This function could be applied to
--   following datasets:
--
--   * Chunked dataset with unlimited dimensions
--
--   * A chunked dataset with fixed dimensions if the new dimension
--     sizes are less than the maximum sizes set with maxdims
setDatasetExtent :: (HasCallStack, IsExtent dim, MonadIO m, MonadThrow m) => Dataset -> dim -> m ()
setDatasetExtent dset dim = propagateError $ do
  p_err         <- ContT $ alloca
  (r_ext,p_ext) <- withEncodedExtent $ encodeExtent dim
  spc    <- ContT $ withDataspace dset
  r_dset <- contCheckCInt p_err "Cannot get rank of dataspace's extent"
          $ h5s_get_simple_extent_ndims (getHID spc)
  when (fromIntegral r_ext /= r_dset) $ throwM $
    Error "Rank of dataset and rank of new extent do not match" []
  contCheckHErr p_err "Failed to set new extent for a dataset"
    $ h5d_set_extent (getHID dset) p_ext




----------------------------------------------------------------
-- Dataspace API
----------------------------------------------------------------

-- | Find rank of dataset or attribute. Returns @Nothing@ for null
--   dataspaces, @Just 0@ for scalars and @Just n@ for rank-N arrays.
rank :: (HasData a, MonadIO m, HasCallStack) => a -> m (Maybe Int)
rank a = liftIO $ withDataspace a dataspaceRank

-- | Compute extent of an dataset or attribute. Returns nothing when
--   extent has unexpected shape. E.g. if 2D array is expected but
--   object is 1D array. Depending on type could be used to obtain
--   size of size and maximum size for any\/each dimensions.
extent
  :: (HasData a, IsDataspace ext, MonadIO m, HasCallStack)
  => a -> m (Either DataspaceParseError ext)
extent a = liftIO $ withDataspace a runParseFromDataspace
