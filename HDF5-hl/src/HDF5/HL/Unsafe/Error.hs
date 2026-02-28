{-# LANGUAGE RecordWildCards #-}
-- |
module HDF5.HL.Unsafe.Error
  ( -- * Exception data type
    Error(..)
  , MajError(..)
  , MinError(..)
  , Message(..)
  , DataspaceParseError(..)
  , AttributeParseError(..)
    -- * API
  , decodeError
  , checkHID
  , checkHErr
  , checkHTri
  , checkCInt
  , checkCLLong
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.C
import Text.Printf
import GHC.Stack
import GHC.Generics (Generic)

import HDF5.C


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


-- | Decode error from HDF5 error stack
decodeError :: HasCallStack => Ptr HID -> String -> IO Error
decodeError p_err msg = evalContT $ do
  hid_err  <- lift  $ peek p_err
  v_stack  <- lift  $ newIORef []
  buf      <- ContT $ allocaArray $ fromIntegral $ msg_size + 1
  let step _ p _ = do
        m_maj    <- peek $ h5e_error_maj_num p
        msgMajor <- do n     <- h5e_get_msg m_maj nullPtr buf msg_size p_err
                       if | n > 0     -> peekCString buf
                          | otherwise -> pure ""
        m_min    <- peek $ h5e_error_min_num p
        msgMinor <- do n     <- h5e_get_msg m_min nullPtr buf msg_size p_err
                       if | n > 0     -> peekCString buf
                          | otherwise -> pure ""
        let msgMajorN = decodeMajError m_maj
            msgMinorN = decodeMinError m_min
        msgFunc  <- peekCString  =<< peek (h5e_error_func_name p)
        msgFile  <- peekCString  =<< peek (h5e_error_file_name p)
        msgDescr <- peekCString  =<< peek (h5e_error_desc      p)
        msgLine  <- fromIntegral <$> peek (h5e_error_line      p)
        modifyIORef' v_stack (Message{..}:)
        pure $ HErr 0
  callback <- ContT $ bracket (makeWalker step) freeHaskellFunPtr
  res      <- lift  $ h5e_walk hid_err H5E_WALK_UPWARD callback nullPtr p_err
  case res of
    HOK      -> lift $ Error msg <$> readIORef v_stack
    HErrored -> pure $ Error (msg ++ internal) []
  where
    -- Error message from major/minor labels are usually short so we
    -- don't need to bother with size discovery
    msg_size = 255
    internal = "\nINTERNAL ERROR: Failed to decode HDF5 error"

checkHID :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HID) -> IO HID
{-# INLINE checkHID #-}
checkHID p_err msg action =
  action p_err >>= \case
    hid | hid < (HID 0) -> throwM =<< decodeError p_err msg
        | otherwise     -> pure hid

checkHErr :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HErr) -> IO ()
{-# INLINE checkHErr #-}
checkHErr p_err msg action =
  action p_err >>= \case
    HOK -> pure ()
    _   -> throwM =<< decodeError p_err msg


checkCInt :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO CInt) -> IO CInt
{-# INLINE checkCInt #-}
checkCInt p_err msg action =
  action p_err >>= \case
    n | n < 0     -> throwM =<< decodeError p_err msg
      | otherwise -> pure n 

checkCLLong :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HSSize) -> IO HSSize
{-# INLINE checkCLLong #-}
checkCLLong p_err msg action =
  action p_err >>= \case
    n | n < 0     -> throwM =<< decodeError p_err msg
      | otherwise -> pure n 

checkHTri :: HasCallStack => Ptr HID -> String -> (Ptr HID -> IO HTri) -> IO Bool
{-# INLINE checkHTri #-}
checkHTri p_err msg action =
  action p_err >>= \case
    HFalse -> pure False
    HTrue  -> pure True
    HFail  -> throwM =<< decodeError p_err msg


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

decodeMajError :: HID -> MajError
decodeMajError h
  | h == c_H5E_ARGS       = MAJ_ARGS
  | h == c_H5E_RESOURCE   = MAJ_RESOURCE
  | h == c_H5E_INTERNAL   = MAJ_INTERNAL
  | h == c_H5E_LIB        = MAJ_LIB
  | h == c_H5E_FILE       = MAJ_FILE
  | h == c_H5E_IO         = MAJ_IO
  | h == c_H5E_FUNC       = MAJ_FUNC
  | h == c_H5E_ID         = MAJ_ID
  | h == c_H5E_CACHE      = MAJ_CACHE
  | h == c_H5E_LINK       = MAJ_LINK
  | h == c_H5E_BTREE      = MAJ_BTREE
  | h == c_H5E_SYM        = MAJ_SYM
  | h == c_H5E_HEAP       = MAJ_HEAP
  | h == c_H5E_OHDR       = MAJ_OHDR
  | h == c_H5E_DATATYPE   = MAJ_DATATYPE
  | h == c_H5E_DATASPACE  = MAJ_DATASPACE
  | h == c_H5E_DATASET    = MAJ_DATASET
  | h == c_H5E_STORAGE    = MAJ_STORAGE
  | h == c_H5E_PLIST      = MAJ_PLIST
  | h == c_H5E_ATTR       = MAJ_ATTR
  | h == c_H5E_PLINE      = MAJ_PLINE
  | h == c_H5E_EFL        = MAJ_EFL
  | h == c_H5E_REFERENCE  = MAJ_REFERENCE
  | h == c_H5E_VFL        = MAJ_VFL
  | h == c_H5E_VOL        = MAJ_VOL
  | h == c_H5E_TST        = MAJ_TST
  | h == c_H5E_RS         = MAJ_RS
  | h == c_H5E_ERROR      = MAJ_ERROR
  | h == c_H5E_SLIST      = MAJ_SLIST
  | h == c_H5E_FSPACE     = MAJ_FSPACE
  | h == c_H5E_SOHM       = MAJ_SOHM
  | h == c_H5E_EARRAY     = MAJ_EARRAY
  | h == c_H5E_FARRAY     = MAJ_FARRAY
  | h == c_H5E_PLUGIN     = MAJ_PLUGIN
  | h == c_H5E_PAGEBUF    = MAJ_PAGEBUF
  | h == c_H5E_CONTEXT    = MAJ_CONTEXT
  | h == c_H5E_MAP        = MAJ_MAP
  | h == c_H5E_EVENTSET   = MAJ_EVENTSET
  | h == c_H5E_NONE_MAJOR = MAJ_NONE_MAJOR
  | otherwise             = MAJ_UNKNOWN

decodeMinError :: HID -> MinError
decodeMinError h
  | h == c_H5E_UNINITIALIZED        = MIN_UNINITIALIZED
  | h == c_H5E_UNSUPPORTED          = MIN_UNSUPPORTED
  | h == c_H5E_BADTYPE              = MIN_BADTYPE
  | h == c_H5E_BADRANGE             = MIN_BADRANGE
  | h == c_H5E_BADVALUE             = MIN_BADVALUE
  | h == c_H5E_NOSPACE              = MIN_NOSPACE
  | h == c_H5E_CANTALLOC            = MIN_CANTALLOC
  | h == c_H5E_CANTCOPY             = MIN_CANTCOPY
  | h == c_H5E_CANTFREE             = MIN_CANTFREE
  | h == c_H5E_ALREADYEXISTS        = MIN_ALREADYEXISTS
  | h == c_H5E_CANTLOCK             = MIN_CANTLOCK
  | h == c_H5E_CANTUNLOCK           = MIN_CANTUNLOCK
  | h == c_H5E_CANTGC               = MIN_CANTGC
  | h == c_H5E_CANTGETSIZE          = MIN_CANTGETSIZE
  | h == c_H5E_OBJOPEN              = MIN_OBJOPEN
  | h == c_H5E_FILEEXISTS           = MIN_FILEEXISTS
  | h == c_H5E_FILEOPEN             = MIN_FILEOPEN
  | h == c_H5E_CANTCREATE           = MIN_CANTCREATE
  | h == c_H5E_CANTOPENFILE         = MIN_CANTOPENFILE
  | h == c_H5E_CANTCLOSEFILE        = MIN_CANTCLOSEFILE
  | h == c_H5E_NOTHDF5              = MIN_NOTHDF5
  | h == c_H5E_BADFILE              = MIN_BADFILE
  | h == c_H5E_TRUNCATED            = MIN_TRUNCATED
  | h == c_H5E_MOUNT                = MIN_MOUNT
  | h == c_H5E_UNMOUNT              = MIN_UNMOUNT
  | h == c_H5E_CANTDELETEFILE       = MIN_CANTDELETEFILE
  | h == c_H5E_CANTLOCKFILE         = MIN_CANTLOCKFILE
  | h == c_H5E_CANTUNLOCKFILE       = MIN_CANTUNLOCKFILE
  | h == c_H5E_SEEKERROR            = MIN_SEEKERROR
  | h == c_H5E_READERROR            = MIN_READERROR
  | h == c_H5E_WRITEERROR           = MIN_WRITEERROR
  | h == c_H5E_CLOSEERROR           = MIN_CLOSEERROR
  | h == c_H5E_OVERFLOW             = MIN_OVERFLOW
  | h == c_H5E_FCNTL                = MIN_FCNTL
  | h == c_H5E_CANTINIT             = MIN_CANTINIT
  | h == c_H5E_ALREADYINIT          = MIN_ALREADYINIT
  | h == c_H5E_CANTRELEASE          = MIN_CANTRELEASE
  | h == c_H5E_BADID                = MIN_BADID
  | h == c_H5E_BADGROUP             = MIN_BADGROUP
  | h == c_H5E_CANTREGISTER         = MIN_CANTREGISTER
  | h == c_H5E_CANTINC              = MIN_CANTINC
  | h == c_H5E_CANTDEC              = MIN_CANTDEC
  | h == c_H5E_NOIDS                = MIN_NOIDS
  | h == c_H5E_CANTFLUSH            = MIN_CANTFLUSH
  | h == c_H5E_CANTUNSERIALIZE      = MIN_CANTUNSERIALIZE
  | h == c_H5E_CANTSERIALIZE        = MIN_CANTSERIALIZE
  | h == c_H5E_CANTTAG              = MIN_CANTTAG
  | h == c_H5E_CANTLOAD             = MIN_CANTLOAD
  | h == c_H5E_PROTECT              = MIN_PROTECT
  | h == c_H5E_NOTCACHED            = MIN_NOTCACHED
  | h == c_H5E_SYSTEM               = MIN_SYSTEM
  | h == c_H5E_CANTINS              = MIN_CANTINS
  | h == c_H5E_CANTPROTECT          = MIN_CANTPROTECT
  | h == c_H5E_CANTUNPROTECT        = MIN_CANTUNPROTECT
  | h == c_H5E_CANTPIN              = MIN_CANTPIN
  | h == c_H5E_CANTUNPIN            = MIN_CANTUNPIN
  | h == c_H5E_CANTMARKDIRTY        = MIN_CANTMARKDIRTY
  | h == c_H5E_CANTMARKCLEAN        = MIN_CANTMARKCLEAN
  | h == c_H5E_CANTMARKUNSERIALIZED = MIN_CANTMARKUNSERIALIZED
  | h == c_H5E_CANTMARKSERIALIZED   = MIN_CANTMARKSERIALIZED
  | h == c_H5E_CANTDIRTY            = MIN_CANTDIRTY
  | h == c_H5E_CANTCLEAN            = MIN_CANTCLEAN
  | h == c_H5E_CANTEXPUNGE          = MIN_CANTEXPUNGE
  | h == c_H5E_CANTRESIZE           = MIN_CANTRESIZE
  | h == c_H5E_CANTDEPEND           = MIN_CANTDEPEND
  | h == c_H5E_CANTUNDEPEND         = MIN_CANTUNDEPEND
  | h == c_H5E_CANTNOTIFY           = MIN_CANTNOTIFY
  | h == c_H5E_LOGGING              = MIN_LOGGING
  | h == c_H5E_CANTCORK             = MIN_CANTCORK
  | h == c_H5E_CANTUNCORK           = MIN_CANTUNCORK
  | h == c_H5E_NOTFOUND             = MIN_NOTFOUND
  | h == c_H5E_EXISTS               = MIN_EXISTS
  | h == c_H5E_CANTENCODE           = MIN_CANTENCODE
  | h == c_H5E_CANTDECODE           = MIN_CANTDECODE
  | h == c_H5E_CANTSPLIT            = MIN_CANTSPLIT
  | h == c_H5E_CANTREDISTRIBUTE     = MIN_CANTREDISTRIBUTE
  | h == c_H5E_CANTSWAP             = MIN_CANTSWAP
  | h == c_H5E_CANTINSERT           = MIN_CANTINSERT
  | h == c_H5E_CANTLIST             = MIN_CANTLIST
  | h == c_H5E_CANTMODIFY           = MIN_CANTMODIFY
  | h == c_H5E_CANTREMOVE           = MIN_CANTREMOVE
  | h == c_H5E_CANTFIND             = MIN_CANTFIND
  | h == c_H5E_LINKCOUNT            = MIN_LINKCOUNT
  | h == c_H5E_VERSION              = MIN_VERSION
  | h == c_H5E_ALIGNMENT            = MIN_ALIGNMENT
  | h == c_H5E_BADMESG              = MIN_BADMESG
  | h == c_H5E_CANTDELETE           = MIN_CANTDELETE
  | h == c_H5E_BADITER              = MIN_BADITER
  | h == c_H5E_CANTPACK             = MIN_CANTPACK
  | h == c_H5E_CANTRESET            = MIN_CANTRESET
  | h == c_H5E_CANTRENAME           = MIN_CANTRENAME
  | h == c_H5E_CANTOPENOBJ          = MIN_CANTOPENOBJ
  | h == c_H5E_CANTCLOSEOBJ         = MIN_CANTCLOSEOBJ
  | h == c_H5E_COMPLEN              = MIN_COMPLEN
  | h == c_H5E_PATH                 = MIN_PATH
  | h == c_H5E_CANTCONVERT          = MIN_CANTCONVERT
  | h == c_H5E_BADSIZE              = MIN_BADSIZE
  | h == c_H5E_CANTCLIP             = MIN_CANTCLIP
  | h == c_H5E_CANTCOUNT            = MIN_CANTCOUNT
  | h == c_H5E_CANTSELECT           = MIN_CANTSELECT
  | h == c_H5E_CANTNEXT             = MIN_CANTNEXT
  | h == c_H5E_BADSELECT            = MIN_BADSELECT
  | h == c_H5E_CANTCOMPARE          = MIN_CANTCOMPARE
  | h == c_H5E_INCONSISTENTSTATE    = MIN_INCONSISTENTSTATE
  | h == c_H5E_CANTAPPEND           = MIN_CANTAPPEND
  | h == c_H5E_CANTGET              = MIN_CANTGET
  | h == c_H5E_CANTSET              = MIN_CANTSET
  | h == c_H5E_DUPCLASS             = MIN_DUPCLASS
  | h == c_H5E_SETDISALLOWED        = MIN_SETDISALLOWED
  | h == c_H5E_TRAVERSE             = MIN_TRAVERSE
  | h == c_H5E_NLINKS               = MIN_NLINKS
  | h == c_H5E_NOTREGISTERED        = MIN_NOTREGISTERED
  | h == c_H5E_CANTMOVE             = MIN_CANTMOVE
  | h == c_H5E_CANTSORT             = MIN_CANTSORT
  | h == c_H5E_MPI                  = MIN_MPI
  | h == c_H5E_MPIERRSTR            = MIN_MPIERRSTR
  | h == c_H5E_CANTRECV             = MIN_CANTRECV
  | h == c_H5E_CANTGATHER           = MIN_CANTGATHER
  | h == c_H5E_NO_INDEPENDENT       = MIN_NO_INDEPENDENT
  | h == c_H5E_CANTRESTORE          = MIN_CANTRESTORE
  | h == c_H5E_CANTCOMPUTE          = MIN_CANTCOMPUTE
  | h == c_H5E_CANTEXTEND           = MIN_CANTEXTEND
  | h == c_H5E_CANTATTACH           = MIN_CANTATTACH
  | h == c_H5E_CANTUPDATE           = MIN_CANTUPDATE
  | h == c_H5E_CANTOPERATE          = MIN_CANTOPERATE
  | h == c_H5E_CANTMERGE            = MIN_CANTMERGE
  | h == c_H5E_CANTREVIVE           = MIN_CANTREVIVE
  | h == c_H5E_CANTSHRINK           = MIN_CANTSHRINK
  | h == c_H5E_NOFILTER             = MIN_NOFILTER
  | h == c_H5E_CALLBACK             = MIN_CALLBACK
  | h == c_H5E_CANAPPLY             = MIN_CANAPPLY
  | h == c_H5E_SETLOCAL             = MIN_SETLOCAL
  | h == c_H5E_NOENCODER            = MIN_NOENCODER
  | h == c_H5E_CANTFILTER           = MIN_CANTFILTER
  | h == c_H5E_SYSERRSTR            = MIN_SYSERRSTR
  | h == c_H5E_OPENERROR            = MIN_OPENERROR
  | h == c_H5E_CANTPUT              = MIN_CANTPUT
  | h == c_H5E_CANTWAIT             = MIN_CANTWAIT
  | h == c_H5E_CANTCANCEL           = MIN_CANTCANCEL
  | h == c_H5E_NONE_MINOR           = MIN_NONE_MINOR
  | otherwise                       = MIN_UNKNOWN
