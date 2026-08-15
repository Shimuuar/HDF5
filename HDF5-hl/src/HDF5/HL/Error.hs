{-# LANGUAGE RecordWildCards #-}
-- |
module HDF5.HL.Error
  ( -- * Exception data type
    Error(..)
  , MajError(..)
  , MinError(..)
  , Message(..)
  , DataspaceParseError(..)
  , AttributeParseError(..)
  ) where

import Control.Exception
import Data.Word
import Text.Printf
import GHC.Stack
import GHC.Generics (Generic)


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Error during HDF5 call
data Error where
  Error :: HasCallStack => String -> [Message] -> Error

-- GHC display exception using show instead of displayException. No
-- way around this. We have to override Show
--
-- See https://mail.haskell.org/pipermail/libraries/2018-May/028813.html
-- for a bit of history
instance Show Error where
  show (Error hs_msg msgs) = unlines $ concat
    [ [ "HDF5 error"
      , hs_msg
      ]
    , [ ' ':' ':prettyCallSite s | s <- getCallStack callStack]
    , displayMsg =<< msgs
    ]
    where
      prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc
      displayMsg Message{..} =
        [ printf "%s (%s:%i): %s" msgFunc msgFile msgLine msgDescr
        , printf "  Major: [%s] %s" (show msgMajorN) msgMajor
        , printf "  Minor: [%s] %s" (show msgMinorN) msgMinor
        ]

data Message = Message
  { msgDescr  :: String
  , msgMajor  :: String
  , msgMajorN :: MajError
  , msgMinor  :: String
  , msgMinorN :: MinError
  , msgLine   :: Int
  , msgFunc   :: String
  , msgFile   :: String
  }
  deriving stock Show

instance Exception Error

----------------------------------------------------------------
-- Error codes
----------------------------------------------------------------

-- | Major error codes for HDF5 error. Here we follow naming
--   conventions used by HDF5
data MajError
  = MAJ_ARGS       -- ^ Invalid arguments to routine
  | MAJ_RESOURCE   -- ^ Resource unavailable
  | MAJ_INTERNAL   -- ^ Internal error (too specific to document in detail)
  | MAJ_LIB        -- ^ General library infrastructure
  | MAJ_FILE       -- ^ File accessibility
  | MAJ_IO         -- ^ Low-level I/O
  | MAJ_FUNC       -- ^ Function entry/exit
  | MAJ_ID         -- ^ Object ID
  | MAJ_CACHE      -- ^ Object cache
  | MAJ_LINK       -- ^ Links
  | MAJ_BTREE      -- ^ B-Tree node
  | MAJ_SYM        -- ^ Symbol table
  | MAJ_HEAP       -- ^ Heap
  | MAJ_OHDR       -- ^ Object header
  | MAJ_DATATYPE   -- ^ Datatype
  | MAJ_DATASPACE  -- ^ Dataspace
  | MAJ_DATASET    -- ^ Dataset
  | MAJ_STORAGE    -- ^ Data storage
  | MAJ_PLIST      -- ^ Property lists
  | MAJ_ATTR       -- ^ Attribute
  | MAJ_PLINE      -- ^ Data filters
  | MAJ_EFL        -- ^ External file list
  | MAJ_REFERENCE  -- ^ References
  | MAJ_VFL        -- ^ Virtual File Layer
  | MAJ_VOL        -- ^ Virtual Object Layer
  | MAJ_TST        -- ^ Ternary Search Trees
  | MAJ_RS         -- ^ Reference Counted Strings
  | MAJ_ERROR      -- ^ Error API
  | MAJ_SLIST      -- ^ Skip Lists
  | MAJ_FSPACE     -- ^ Free Space Manager
  | MAJ_SOHM       -- ^ Shared Object Header Messages
  | MAJ_EARRAY     -- ^ Extensible Array
  | MAJ_FARRAY     -- ^ Fixed Array
  | MAJ_PLUGIN     -- ^ Plugin for dynamically loaded library
  | MAJ_PAGEBUF    -- ^ Page Buffering
  | MAJ_CONTEXT    -- ^ API Context
  | MAJ_MAP        -- ^ Map
  | MAJ_EVENTSET   -- ^ Event Set
  | MAJ_NONE_MAJOR -- ^ No error
  | MAJ_UNKNOWN    -- ^ We failed to map error code from HDF5 to
                   --   haskell type. If it appears in error this means
                   --   there's bug in bindings.
  deriving stock (Show,Generic)

-- | Minor error codes for HDF5 error. Here we follow naming
--   conventions used by HDF5
data MinError
  = MIN_UNINITIALIZED        -- ^ Information is uinitialized
  | MIN_UNSUPPORTED          -- ^ Feature is unsupported
  | MIN_BADTYPE              -- ^ Inappropriate type
  | MIN_BADRANGE             -- ^ Out of range
  | MIN_BADVALUE             -- ^ Bad value
  | MIN_NOSPACE              -- ^ No space available for allocation
  | MIN_CANTALLOC            -- ^ Can't allocate space
  | MIN_CANTCOPY             -- ^ Unable to copy object
  | MIN_CANTFREE             -- ^ Unable to free object
  | MIN_ALREADYEXISTS        -- ^ Object already exists
  | MIN_CANTLOCK             -- ^ Unable to lock object
  | MIN_CANTUNLOCK           -- ^ Unable to unlock object
  | MIN_CANTGC               -- ^ Unable to garbage collect
  | MIN_CANTGETSIZE          -- ^ Unable to compute size
  | MIN_OBJOPEN              -- ^ Object is already open
  | MIN_FILEEXISTS           -- ^ File already exists
  | MIN_FILEOPEN             -- ^ File already open
  | MIN_CANTCREATE           -- ^ Unable to create file
  | MIN_CANTOPENFILE         -- ^ Unable to open file
  | MIN_CANTCLOSEFILE        -- ^ Unable to close file
  | MIN_NOTHDF5              -- ^ Not an HDF5 file
  | MIN_BADFILE              -- ^ Bad file ID accessed
  | MIN_TRUNCATED            -- ^ File has been truncated
  | MIN_MOUNT                -- ^ File mount error
  | MIN_UNMOUNT              -- ^ File unmount error
  | MIN_CANTDELETEFILE       -- ^ Unable to delete file
  | MIN_CANTLOCKFILE         -- ^ Unable to lock file
  | MIN_CANTUNLOCKFILE       -- ^ Unable to unlock file
  | MIN_SEEKERROR            -- ^ Seek failed
  | MIN_READERROR            -- ^ Read failed
  | MIN_WRITEERROR           -- ^ Write failed
  | MIN_CLOSEERROR           -- ^ Close failed
  | MIN_OVERFLOW             -- ^ Address overflowed
  | MIN_FCNTL                -- ^ File control (fcntl) failed
  | MIN_CANTINIT             -- ^ Unable to initialize object
  | MIN_ALREADYINIT          -- ^ Object already initialized
  | MIN_CANTRELEASE          -- ^ Unable to release object
  | MIN_BADID                -- ^ Unable to find ID information (already closed?)
  | MIN_BADGROUP             -- ^ Unable to find ID group information
  | MIN_CANTREGISTER         -- ^ Unable to register new ID
  | MIN_CANTINC              -- ^ Unable to increment reference count
  | MIN_CANTDEC              -- ^ Unable to decrement reference count
  | MIN_NOIDS                -- ^ Out of IDs for group
  | MIN_CANTFLUSH            -- ^ Unable to flush data from cache
  | MIN_CANTUNSERIALIZE      -- ^ Unable to mark metadata as unserialized
  | MIN_CANTSERIALIZE        -- ^ Unable to serialize data from cache
  | MIN_CANTTAG              -- ^ Unable to tag metadata in the cache
  | MIN_CANTLOAD             -- ^ Unable to load metadata into cache
  | MIN_PROTECT              -- ^ Protected metadata error
  | MIN_NOTCACHED            -- ^ Metadata not currently cached
  | MIN_SYSTEM               -- ^ Internal error detected
  | MIN_CANTINS              -- ^ Unable to insert metadata into cache
  | MIN_CANTPROTECT          -- ^ Unable to protect metadata
  | MIN_CANTUNPROTECT        -- ^ Unable to unprotect metadata
  | MIN_CANTPIN              -- ^ Unable to pin cache entry
  | MIN_CANTUNPIN            -- ^ Unable to un-pin cache entry
  | MIN_CANTMARKDIRTY        -- ^ Unable to mark a pinned entry as dirty
  | MIN_CANTMARKCLEAN        -- ^ Unable to mark a pinned entry as clean
  | MIN_CANTMARKUNSERIALIZED -- ^ Unable to mark an entry as unserialized
  | MIN_CANTMARKSERIALIZED   -- ^ Unable to mark an entry as serialized
  | MIN_CANTDIRTY            -- ^ Unable to mark metadata as dirty
  | MIN_CANTCLEAN            -- ^ Unable to mark metadata as clean
  | MIN_CANTEXPUNGE          -- ^ Unable to expunge a metadata cache entry
  | MIN_CANTRESIZE           -- ^ Unable to resize a metadata cache entry
  | MIN_CANTDEPEND           -- ^ Unable to create a flush dependency
  | MIN_CANTUNDEPEND         -- ^ Unable to destroy a flush dependency
  | MIN_CANTNOTIFY           -- ^ Unable to notify object about action
  | MIN_LOGGING              -- ^ Failure in the cache logging framework
  | MIN_CANTCORK             -- ^ Unable to cork an object
  | MIN_CANTUNCORK           -- ^ Unable to uncork an object
  | MIN_NOTFOUND             -- ^ Object not found
  | MIN_EXISTS               -- ^ Object already exists
  | MIN_CANTENCODE           -- ^ Unable to encode value
  | MIN_CANTDECODE           -- ^ Unable to decode value
  | MIN_CANTSPLIT            -- ^ Unable to split node
  | MIN_CANTREDISTRIBUTE     -- ^ Unable to redistribute records
  | MIN_CANTSWAP             -- ^ Unable to swap records
  | MIN_CANTINSERT           -- ^ Unable to insert object
  | MIN_CANTLIST             -- ^ Unable to list node
  | MIN_CANTMODIFY           -- ^ Unable to modify record
  | MIN_CANTREMOVE           -- ^ Unable to remove object
  | MIN_CANTFIND             -- ^ Unable to check for record
  | MIN_LINKCOUNT            -- ^ Bad object header link count
  | MIN_VERSION              -- ^ Wrong version number
  | MIN_ALIGNMENT            -- ^ Alignment error
  | MIN_BADMESG              -- ^ Unrecognized message
  | MIN_CANTDELETE           -- ^ Can't delete message
  | MIN_BADITER              -- ^ Iteration failed
  | MIN_CANTPACK             -- ^ Can't pack messages
  | MIN_CANTRESET            -- ^ Can't reset object
  | MIN_CANTRENAME           -- ^ Unable to rename object
  | MIN_CANTOPENOBJ          -- ^ Can't open object
  | MIN_CANTCLOSEOBJ         -- ^ Can't close object
  | MIN_COMPLEN              -- ^ Name component is too long
  | MIN_PATH                 -- ^ Problem with path to object
  | MIN_CANTCONVERT          -- ^ Can't convert datatypes
  | MIN_BADSIZE              -- ^ Bad size for object
  | MIN_CANTCLIP             -- ^ Can't clip hyperslab region
  | MIN_CANTCOUNT            -- ^ Can't count elements
  | MIN_CANTSELECT           -- ^ Can't select hyperslab
  | MIN_CANTNEXT             -- ^ Can't move to next iterator location
  | MIN_BADSELECT            -- ^ Invalid selection
  | MIN_CANTCOMPARE          -- ^ Can't compare objects
  | MIN_INCONSISTENTSTATE    -- ^ Internal states are inconsistent
  | MIN_CANTAPPEND           -- ^ Can't append object
  | MIN_CANTGET              -- ^ Can't get value
  | MIN_CANTSET              -- ^ Can't set value
  | MIN_DUPCLASS             -- ^ Duplicate class name in parent class
  | MIN_SETDISALLOWED        -- ^ Disallowed operation
  | MIN_TRAVERSE             -- ^ Link traversal failure
  | MIN_NLINKS               -- ^ Too many soft links in path
  | MIN_NOTREGISTERED        -- ^ Link class not registered
  | MIN_CANTMOVE             -- ^ Can't move object
  | MIN_CANTSORT             -- ^ Can't sort objects
  | MIN_MPI                  -- ^ Some MPI function failed
  | MIN_MPIERRSTR            -- ^ MPI Error String
  | MIN_CANTRECV             -- ^ Can't receive data
  | MIN_CANTGATHER           -- ^ Can't gather data
  | MIN_NO_INDEPENDENT       -- ^ Can't perform independent IO
  | MIN_CANTRESTORE          -- ^ Can't restore condition
  | MIN_CANTCOMPUTE          -- ^ Can't compute value
  | MIN_CANTEXTEND           -- ^ Can't extend heap's space
  | MIN_CANTATTACH           -- ^ Can't attach object
  | MIN_CANTUPDATE           -- ^ Can't update object
  | MIN_CANTOPERATE          -- ^ Can't operate on object
  | MIN_CANTMERGE            -- ^ Can't merge objects
  | MIN_CANTREVIVE           -- ^ Can't revive object
  | MIN_CANTSHRINK           -- ^ Can't shrink container
  | MIN_NOFILTER             -- ^ Requested filter is not available
  | MIN_CALLBACK             -- ^ Callback failed
  | MIN_CANAPPLY             -- ^ Error from filter 'can apply' callback
  | MIN_SETLOCAL             -- ^ Error from filter 'set local' callback
  | MIN_NOENCODER            -- ^ Filter present but encoding disabled
  | MIN_CANTFILTER           -- ^ Filter operation failed
  | MIN_SYSERRSTR            -- ^ System error message
  | MIN_OPENERROR            -- ^ Can't open directory or file
  | MIN_CANTPUT              -- ^ Can't put value
  | MIN_CANTWAIT             -- ^ Can't wait on operation
  | MIN_CANTCANCEL           -- ^ Can't cancel operation
  | MIN_NONE_MINOR           -- ^ No error
  | MIN_UNKNOWN              -- ^ Failed to decode HDF5 error code.
  deriving stock (Show,Generic)

-- | Error during conversion of dataspace's size to haskell data type.
data DataspaceParseError
  = BadRank ![(Word64,Word64)]  -- ^ Has invalid shape
  | UnexpectedNull              -- ^ Cannot convert NULL dataspace to haskell type
  | BadIndex ![(Word64,Word64)] -- ^ Cannot convert index to haskell data type
  deriving stock Show

instance Exception DataspaceParseError


-- | Error during parsing of attribute
data AttributeParseError
  = MissingAttribute    !String -- ^ Attribute
  | AttributeParseError !String -- ^ Any other error
  deriving stock Show

instance Exception AttributeParseError
