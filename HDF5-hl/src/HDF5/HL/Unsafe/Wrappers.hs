{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Data types for working with HDF5 files
module HDF5.HL.Unsafe.Wrappers
  ( -- * Files and groups
    File(..)
  , OpenMode(..)
  , CreateMode(..)
  , Group(..)
  , Dataset(..)
  , Attribute(..)
  , Dataspace(..)
  , Type(..)
  , PropertyHID(..)
    -- * Type classes
  , Closable(..)
  , IsObject(..)
  , IsDirectory
  , HasAttrs
  , HasData(..)
  ) where

import Data.Coerce
import Foreign.Ptr
import GHC.Stack

import HDF5.C
import HDF5.HL.Internal.Check
import HDF5.HL.Internal.Enum
import HDF5.HL.Unsafe.Types
import HDF5.HL.Monad


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Some HDF5 object.
class IsObject a where
  getHID        :: a -> HID
  unsafeFromHID :: HID -> a


-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class IsObject a => IsDirectory a

-- | Objects which could have attributes attached, such as files and groups
class IsObject a => HasAttrs a

-- | HDF5 entities which contains data that could be
class IsObject a => HasData a where
  -- | Get type of object
  getTypeHDF      :: HasCallStack => a -> Hdf5M s (Type s)
  -- | Get dataspace associated with object
  getDataspaceHDF :: HasCallStack => a -> Hdf5M s (Dataspace s)
  -- | Read all content of object
  unsafeReadAll  :: HasCallStack
                 => a       -- ^ Object handle
                 -> Type s  -- ^ Type of in-memory elements
                 -> Ptr x   -- ^ Buffer to read to
                 -> Hdf5M s ()
  -- | Write full dataset at once
  unsafeWriteAll :: HasCallStack
                 => a       -- ^ Object handle
                 -> Type s  -- ^ Type of in-memory elements
                 -> Ptr x   -- ^ Buffer with data
                 -> Hdf5M s ()



----------------------------------------------------------------
-- Files, groups, datasets
----------------------------------------------------------------

-- | Handle for working with HDF5 file. It also serves as root
--   directory of a file when group is expected. See 'IsDirectory'.
newtype File = File HID
  deriving stock (Show,Eq,Ord)

-- | Handle for working with group (directory).
newtype Group = Group HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataset. It's dense N-dimensional array of
--   elements. Dimensions of array are called 'Dataspace' in HDF5
--   terminology. Extent of already existing dataset could be
--   changed. Wide range of 'Type's are supported: fixed width
--   integers, IEEE754 floating point, fixed size arrays, structures,
--   enumerations.
newtype Dataset = Dataset HID
  deriving stock (Show,Eq,Ord)

-- | Handle for attribute attached to file, group, dataset. Attribute
--   is named value: scalar or small array.
newtype Attribute = Attribute HID
  deriving stock (Show,Eq,Ord)

-- | Handle for dataspace. It defines number of dimensions and size of
--   each dimension for datasets and attributes. Each dataspace has
--   size and maximum size which could be larger. Special value
--   'UNLIMITED' is used to denote that particular dimension is unbounded.
--   Datasets in which size and maximum size are different must be chunked.
--
--   It's convenient to represent dataspaces using haskell data
--   type. Type classs 'HDF5.HL.Dataspace.IsExtent' and
--   'HDF5.HL.Dataspace.IsDataspace' are used to convert haskell
--   values to dataspaces and parse dimension data back.
newtype Dataspace s = Dataspace HID
  deriving stock (Show,Eq,Ord)

-- | Property list for values of type @p@.
newtype PropertyHID s p = PropertyHID HID
  deriving stock (Show,Eq,Ord)

----------------

instance IsObject File where
  getHID        = coerce
  unsafeFromHID = coerce
instance IsDirectory File
instance HasAttrs    File

----------------

instance IsObject Group where
  getHID        = coerce
  unsafeFromHID = coerce
instance IsDirectory Group
instance HasAttrs    Group

----------------

instance IsObject Dataset where
  getHID        = coerce
  unsafeFromHID = coerce
instance HasAttrs Dataset
instance HasData  Dataset where
  getTypeHDF (Dataset hid) = withFrozenCallStack
    $ boundCheckHID "Cannot get type of dataset" Type
    $ h5d_get_type hid
  getDataspaceHDF (Dataset hid) = withFrozenCallStack
    $ boundCheckHID "Cannot read dataset's dataspace" Dataspace
    $ h5d_get_space hid
  unsafeReadAll (Dataset hid) ty buf
    = contCheckHErr "Reading dataset data failed"
    $ h5d_read hid (getTypeHID ty)
        h5s_ALL h5s_ALL H5P_DEFAULT (castPtr buf)
  unsafeWriteAll (Dataset hid) ty buf
    = contCheckHErr "Writing dataset data failed"
    $ h5d_write hid (getTypeHID ty)
        h5s_ALL h5s_ALL H5P_DEFAULT buf


----------------

instance IsObject Attribute where
  getHID        = coerce
  unsafeFromHID = coerce
instance HasData Attribute where
  getTypeHDF (Attribute hid) = withFrozenCallStack
    $ boundCheckHID "Cannot get type of attribute" Type
    $ h5a_get_type hid
  getDataspaceHDF (Attribute hid) = withFrozenCallStack
    $ boundCheckHID "Cannot get attribute's dataspace" Dataspace
    $ h5a_get_space hid
  unsafeReadAll (Attribute hid) ty buf
    = contCheckHErr "Reading attribute data failed"
    $ h5a_read hid (getTypeHID ty) (castPtr buf)
  unsafeWriteAll (Attribute hid) ty buf
    = contCheckHErr "Writing Attribute data failed"
    $ h5a_write hid (getTypeHID ty) (castPtr buf)


instance IsObject (Dataspace s) where
  getHID        = coerce
  unsafeFromHID = coerce

instance IsObject (PropertyHID s p) where
  getHID        = coerce
  unsafeFromHID = coerce


----------------------------------------------------------------

instance Closable File where
  basicClose (File hid)
    = runHdf5M
    $ contCheckHErr "Failed to close File"
    $ h5f_close hid

instance Closable Dataset where
  basicClose (Dataset hid)
    = runHdf5M
    $ contCheckHErr "Failed to close Dataset"
    $ h5d_close hid

instance Closable Attribute where
  basicClose (Attribute hid)
    = runHdf5M
    $ contCheckHErr "Failed to close Attribute"
    $ h5a_close hid

instance Closable (Dataspace s) where
  basicClose (Dataspace hid)
    = runHdf5M
    $ contCheckHErr "Failed to close Dataspace"
    $ h5s_close hid

instance Closable Group where
  basicClose (Group hid)
    = runHdf5M
    $ contCheckHErr "Failed to close Group"
    $ h5g_close hid

instance Closable (PropertyHID s p) where
  basicClose (PropertyHID hid)
    = runHdf5M
    $ contCheckHErr "Failed to close PropertyHID"
    $ h5p_close hid
