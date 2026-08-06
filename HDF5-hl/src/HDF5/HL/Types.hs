
-- |
-- API for working with HDF5 data types. We treat them as immutable
-- while they're mutable in HDF5.
module HDF5.HL.Types
  ( -- * Operations on types
    Type
  , withType
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
  , pattern Array
  , makePackedRecord
  , makeEnumeration
    -- * Element
  , Element(..)
  ) where


import HDF5.HL.Unsafe.Types
