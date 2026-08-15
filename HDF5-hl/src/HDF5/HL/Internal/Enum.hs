{-# LANGUAGE TypeFamilies #-}
-- |
-- Mapping of C enums from HDF to haskell data types
module HDF5.HL.Internal.Enum
  ( -- * Type classes
    HDF5Enum(..)
  , HDF5Param(..)
    -- * Enumerations
  , OpenMode(..)
  , CreateMode(..)
  , Sign(..)
  , Class(..)
  , Layout(..)
  ) where

import Foreign.C.Types
import HDF5.C


-- | Conversion to and from corresponding C enumeration
class HDF5Enum a where
  type CEnum a
  fromCEnum :: CEnum a -> Maybe a
  toCEnum   :: a -> CEnum a

-- | Type class for values which could be converted to C
--   parameters. This type class is for value which only used as parametersq
class HDF5Param a where
  type CParam a
  toCParam :: a -> CParam a



-- | Mode for opening existing files
data OpenMode
  = OpenRO     -- ^ Open file in read-only mode.
  | OpenRW     -- ^ Open file in read-write mode. Throws exception if
               --   file doesn't exists.
  | OpenAppend -- ^ Open file in read-write mode or create it if it doesn't exists.
  deriving stock (Show, Eq)

-- | Mode for creating new files
data CreateMode
  = CreateTrunc -- ^ Truncate file, if it already exists, erasing all
                --   data previously stored in the file
  | CreateExcl  -- ^ Fail if file already exists
  deriving stock (Show, Eq)

instance HDF5Param CreateMode where
  type CParam CreateMode = CUInt
  toCParam CreateTrunc = h5f_ACC_TRUNC
  toCParam CreateExcl  = h5f_ACC_EXCL


-- | Whether integral value is signed or not
data Sign
  = Signed
  | Unsigned
  deriving stock (Show,Eq,Ord)

instance HDF5Enum Sign where
  type CEnum Sign = H5TSign
  toCEnum Signed   = H5T_SGN_2
  toCEnum Unsigned = H5T_SGN_NONE
  fromCEnum = \case
    H5T_SGN_2    -> Just Signed
    H5T_SGN_NONE -> Just Unsigned
    _              -> Nothing


-- | Class of type
data Class
  = NoClass
  | Integer
  | Float
  | Time
  | String
  | BitField
  | Opaque
  | Compound
  | Reference
  | Enum
  | Vlen
  | ClsArray
  deriving stock (Show,Eq,Ord)

instance HDF5Enum Class where
  type CEnum Class = H5TClass
  toCEnum = \case
    NoClass   -> H5T_NO_CLASS
    Integer   -> H5T_INTEGER
    Float     -> H5T_FLOAT
    Time      -> H5T_TIME
    String    -> H5T_STRING
    BitField  -> H5T_BITFIELD
    Opaque    -> H5T_OPAQUE
    Compound  -> H5T_COMPOUND
    Reference -> H5T_REFERENCE
    Enum      -> H5T_ENUM
    Vlen      -> H5T_VLEN
    ClsArray  -> H5T_ARRAY
  fromCEnum = \case
    H5T_NO_CLASS  -> Just NoClass
    H5T_INTEGER   -> Just Integer
    H5T_FLOAT     -> Just Float
    H5T_TIME      -> Just Time
    H5T_STRING    -> Just String
    H5T_BITFIELD  -> Just BitField
    H5T_OPAQUE    -> Just Opaque
    H5T_COMPOUND  -> Just Compound
    H5T_REFERENCE -> Just Reference
    H5T_ENUM      -> Just Enum
    H5T_VLEN      -> Just Vlen
    H5T_ARRAY     -> Just ClsArray
    _             -> Nothing

-- | Layout of dataset
data Layout
  = Compact   -- ^ Store raw data in the dataset object header in
              --   file. This should only be used for datasets with
              --   small amounts of raw data. The raw data size limit
              --   is 64K (65520 bytes). Attempting to create a
              --   dataset with raw data larger than this limit will
              --   cause runtime exception.
  | Contigous -- ^ Store raw data separately from the object header in
              --   one large chunk in the file.
  | Chunked   -- ^ Store raw data separately from the object header as
              --   chunks of data in separate locations in the file.

instance HDF5Enum Layout where
  type CEnum Layout = H5DLayout
  toCEnum = \case
    Compact   -> H5D_COMPACT
    Contigous -> H5D_CONTIGUOUS
    Chunked   -> H5D_CHUNKED
  fromCEnum = \case
    H5D_COMPACT    -> Just Compact
    H5D_CONTIGUOUS -> Just Contigous
    H5D_CHUNKED    -> Just Chunked
    _              -> Nothing
