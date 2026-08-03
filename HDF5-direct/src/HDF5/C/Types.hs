{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Newtype wrappers over data types defined by C API of HDF5.
module HDF5.C.Types
  ( -- * Data types and patters
    HID(..)
  , HTri(..)
  , pattern HTrue
  , pattern HFalse
  , pattern HFail
  , HErr(..)
  , pattern HOK
  , pattern HErrored
  , HSize
  , HSSize
    -- * Enumerations
  , H5Index(..)
  , pattern H5_INDEX_UNKNOWN
  , pattern H5_INDEX_NAME
  , pattern H5_INDEX_CRT_ORDER
  , pattern H5_INDEX_N
  , H5IterOrder(..)
  , pattern H5_ITER_UNKNOWN
  , pattern H5_ITER_INC
  , pattern H5_ITER_DEC
  , pattern H5_ITER_NATIVE
  , pattern H5_ITER_N
    -- * IO wrapper
  , HIO
  , HDF5IO(..)
  , lockHDF5
  ) where

import Control.Concurrent.MVar
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe
import GHC.Generics (Generic)


----------------------------------------------------------------
-- Newtype wrappers
----------------------------------------------------------------

-- | Type of IDs to return to users
newtype HID = HID Int64
  deriving stock   (Show,Eq,Ord,Generic)
  deriving newtype (Storable)

-- | Three-valued Boolean type. Functions that return htri_t however
--   return zero (false), positive (true), or negative (failure).
newtype HTri = HTri CInt
  deriving stock (Show,Eq,Ord,Generic)

pattern HTrue, HFalse, HFail :: HTri
pattern HFalse = HTri 0
pattern HTrue  <- HTri ((>0) -> True)
pattern HFail  <- HTri ((<0) -> True)
{-# COMPLETE HTrue, HFalse, HFail #-}

-- | Status return values. Failed integer functions in HDF5 result
--   almost always in a negative value (unsigned failing functions
--   sometimes return zero for failure) while successful return is
--   non-negative (often zero). The negative failure value is most
--   commonly -1, but don't bet on it.
newtype HErr = HErr CInt
  deriving stock (Show,Eq,Ord,Generic)

pattern HErrored, HOK :: HErr
pattern HErrored <- HErr ((<0)  -> True)
pattern HOK      <- HErr ((>=0) -> True)
{-# COMPLETE HErrored, HOK #-}


type HSize  = Word64
type HSSize = Int64


-- | We use standard approach where last parameter of wrapper function
--   is always pointer where we return error stack in case of error.
type HIO a = Ptr HID -> HDF5IO a

-- | Wrapper for HDF5 function calls. They have to be protected by
--   lock this wrapper is used to check for that
newtype HDF5IO a = HDF5IO { unsafeHDF5 :: IO a }

lock :: MVar ()
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}

-- | Run HDF5 function call under protected by global mutex
lockHDF5 :: HDF5IO a -> IO a
lockHDF5 (HDF5IO io) = withMVar lock $ \_ -> io


----------------------------------------------------------------
-- Enumerations
----------------------------------------------------------------


-- | The types of indices on links in groups\/attributes on
--   objects. Primarily used for @<do> <foo> by index@ routines and
--   for iterating over links in groups\/attributes on objects.
newtype H5Index = H5Index CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5_INDEX_UNKNOWN"   h5_INDEX_UNKNOWN   :: H5Index
foreign import capi "hdf5.h value H5_INDEX_NAME"      h5_INDEX_NAME      :: H5Index
foreign import capi "hdf5.h value H5_INDEX_CRT_ORDER" h5_INDEX_CRT_ORDER :: H5Index
foreign import capi "hdf5.h value H5_INDEX_N"         h5_INDEX_N         :: H5Index

-- | Unknown index type
pattern H5_INDEX_UNKNOWN :: H5Index
pattern H5_INDEX_UNKNOWN <- ((==h5_INDEX_UNKNOWN) -> True) where H5_INDEX_UNKNOWN = h5_INDEX_UNKNOWN

-- | Index on names
pattern H5_INDEX_NAME :: H5Index
pattern H5_INDEX_NAME <- ((==h5_INDEX_NAME) -> True) where H5_INDEX_NAME = h5_INDEX_NAME

-- | Index on creation order
pattern H5_INDEX_CRT_ORDER :: H5Index
pattern H5_INDEX_CRT_ORDER <- ((==h5_INDEX_CRT_ORDER) -> True) where H5_INDEX_CRT_ORDER = h5_INDEX_CRT_ORDER

-- | Number of indices defined
pattern H5_INDEX_N :: H5Index
pattern H5_INDEX_N <- ((==h5_INDEX_N) -> True) where H5_INDEX_N = h5_INDEX_N


-- | Common iteration orders
newtype H5IterOrder = H5IterOrder CInt
  deriving (Show,Eq,Ord)

foreign import capi "hdf5.h value H5_ITER_UNKNOWN" h5_ITER_UNKNOWN :: H5IterOrder
foreign import capi "hdf5.h value H5_ITER_INC"     h5_ITER_INC     :: H5IterOrder
foreign import capi "hdf5.h value H5_ITER_DEC"     h5_ITER_DEC     :: H5IterOrder
foreign import capi "hdf5.h value H5_ITER_NATIVE"  h5_ITER_NATIVE  :: H5IterOrder
foreign import capi "hdf5.h value H5_ITER_N"       h5_ITER_N       :: H5IterOrder


-- | Unknown order
pattern H5_ITER_UNKNOWN :: H5IterOrder
pattern H5_ITER_UNKNOWN <- ((==h5_ITER_UNKNOWN) -> True) where H5_ITER_UNKNOWN = h5_ITER_UNKNOWN

-- | Increasing order
pattern H5_ITER_INC :: H5IterOrder
pattern H5_ITER_INC <- ((==h5_ITER_INC) -> True) where H5_ITER_INC = h5_ITER_INC

-- | Decreasing order
pattern H5_ITER_DEC :: H5IterOrder
pattern H5_ITER_DEC <- ((==h5_ITER_DEC) -> True) where H5_ITER_DEC = h5_ITER_DEC

-- | No particular order, whatever is fastest
pattern H5_ITER_NATIVE :: H5IterOrder
pattern H5_ITER_NATIVE <- ((==h5_ITER_NATIVE) -> True) where H5_ITER_NATIVE = h5_ITER_NATIVE

-- | Number of iteration orders
pattern H5_ITER_N :: H5IterOrder
pattern H5_ITER_N <- ((==h5_ITER_N) -> True) where H5_ITER_N = h5_ITER_N
