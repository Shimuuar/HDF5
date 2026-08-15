{-# LANGUAGE RecordWildCards #-}
-- |
-- Check FFI calls for errors
module HDF5.HL.Internal.Check where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.IORef
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Foreign.C
import GHC.Stack

import HDF5.C
import HDF5.HL.Error
import HDF5.HL.Monad
import HDF5.HL.Internal.Classes


-- | Decode error from HDF5 error stack
decodeError :: HasCallStack => Ptr HID -> String -> IO Error
decodeError p_err msg = evalContT $ do
  hid_err  <- lift  $ peek p_err
  v_stack  <- lift  $ newIORef []
  buf      <- ContT $ allocaArray $ fromIntegral $ msg_size + 1
  -- NOTE: In callback we already hold global lock. We must not
  --       reacquire it or else we'll deadlock.
  let step _ p _ = do
        m_maj    <- peek $ h5e_error_maj_num p
        msgMajor <- do n <- unsafeHDF5 $ h5e_get_msg m_maj nullPtr buf msg_size p_err
                       if | n > 0     -> peekCString buf
                          | otherwise -> pure ""
        m_min    <- peek $ h5e_error_min_num p
        msgMinor <- do n <- unsafeHDF5 $ h5e_get_msg m_min nullPtr buf msg_size p_err
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
  res      <- lift  $ lockHDF5 $ h5e_walk hid_err H5E_WALK_UPWARD callback nullPtr p_err
  case res of
    HOK      -> lift $ Error msg <$> readIORef v_stack
    HErrored -> pure $ Error (msg ++ internal) []
  where
    -- Error message from major/minor labels are usually short so we
    -- don't need to bother with size discovery
    msg_size = 255
    internal = "\nINTERNAL ERROR: Failed to decode HDF5 error"


----------------------------------------------------------------
-- Handling of errors
----------------------------------------------------------------


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


contCheckHID :: String -> (Ptr HID -> HDF5IO HID) -> Hdf5M s HID
{-# INLINE contCheckHID #-}
contCheckHID msg action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err) >>= \case
    hid | hid < (HID 0) -> abort msg
        | otherwise     -> pure hid

boundCheckHID :: Closable a => String -> (HID -> a) -> (Ptr HID -> HDF5IO HID) -> Hdf5M s a
{-# INLINE boundCheckHID #-}
boundCheckHID msg mk ffi_call = do
  p_err <- askPErr
  r     <- liftBracket $ bracket
    (lockHDF5 (ffi_call p_err) >>= \case
      hid | hid < (HID 0) -> pure $ Nothing
          | otherwise     -> pure $ Just $ mk hid
    )
    (\case
        Nothing -> pure ()
        Just  a -> basicClose a
    )
  case r of
    Nothing -> abort msg
    Just a  -> pure a




contCheckHErr :: String -> (Ptr HID -> HDF5IO HErr) -> Hdf5M s ()
{-# INLINE contCheckHErr #-}
contCheckHErr msg action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err) >>= \case
    HOK -> pure ()
    _   -> abort msg

contCheckCInt :: String -> (Ptr HID -> HDF5IO CInt) -> Hdf5M s CInt
{-# INLINE contCheckCInt #-}
contCheckCInt msg action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err) >>= \case
    n | n < 0     -> abort msg
      | otherwise -> pure n

contCheckCSize :: String -> (Ptr HID -> HDF5IO CSize) -> Hdf5M s CSize
{-# INLINE contCheckCSize #-}
contCheckCSize msg action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err) >>= \case
    n | n < 0     -> abort msg
      | otherwise -> pure n

contCheckCLLong :: String -> (Ptr HID -> HDF5IO HSSize) -> Hdf5M s HSSize
{-# INLINE contCheckCLLong #-}
contCheckCLLong msg action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err) >>= \case
    n | n < 0     -> abort msg
      | otherwise -> pure n

contCheckHTri :: String -> (Ptr HID -> HDF5IO HTri) -> Hdf5M s Bool
{-# INLINE contCheckHTri #-}
contCheckHTri msg action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err) >>= \case
    HFalse -> pure False
    HTrue  -> pure True
    HFail  -> abort msg

contUnchecked :: (Ptr HID -> HDF5IO a) -> Hdf5M s a
{-# INLINE contUnchecked #-}
contUnchecked action = do
  p_err <- askPErr
  liftIO (lockHDF5 $ action p_err)


abort :: String -> Hdf5M s a
abort msg = do
  p_err <- askPErr
  throwHdf5 =<< liftIO (decodeError p_err msg)
