-- |
-- Type classes 
module HDF5.HL.Internal.Classes where

import Foreign.Ptr
import GHC.Stack

import HDF5.C
import HDF5.HL.Monad


-- | Some HDF5 object.
class IsObject a where
  getHID        :: a -> HID
  unsafeFromHID :: HID -> a

-- | HDF5 entities that could be used in context where group is
--   expected: groups, files (root group is used).
class IsObject a => IsDirectory a

-- | Objects which could have attributes attached, such as files and groups
class IsObject a => HasAttrs a

-- | Most value (files, groups, datasets, etc.) should be closed
--   explicitly in order to avoid resource leaks. This is utility
--   class which allows to use same function to all of them.
class Closable a where
  basicClose :: HasCallStack => a -> IO ()

