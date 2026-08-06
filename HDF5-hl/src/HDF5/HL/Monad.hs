-- |
module HDF5.HL.Monad
  ( Hdf5M(..)
  , runHdf5M
  , runLiftHdf5M
  , runHdf5MEither
  , scopeHdfFinalizers
  , askPErr
  , liftBracket
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.Marshal

import HDF5.C.Types
import HDF5.HL.Unsafe.ErrorTy

-- | Monad for working with HDF5 code
newtype Hdf5M a = Hdf5M (forall r. Ptr HID -> ContT (Either Error r) IO a)
  deriving stock Functor

runHdf5M :: Hdf5M a -> IO a
runHdf5M = either throwM pure <=< runHdf5MEither

runLiftHdf5M :: (MonadIO m, MonadThrow m) => Hdf5M a -> m a
runLiftHdf5M = either throwM pure <=< liftIO . runHdf5MEither

runHdf5MEither :: Hdf5M a -> IO (Either Error a)
runHdf5MEither (Hdf5M action) = alloca $ \ptr -> runContT (action ptr) (pure . Right)

instance Applicative Hdf5M where
  pure a = Hdf5M (const (pure a))
  Hdf5M f <*> Hdf5M a = Hdf5M $ \ptr -> f ptr <*> a ptr
  liftA2 f (Hdf5M a) (Hdf5M b) = Hdf5M $ \ptr -> liftA2 f (a ptr) (b ptr)

instance Monad Hdf5M where
  Hdf5M m >>= f = Hdf5M $ \ptr -> do
    a <- m ptr
    case f a of Hdf5M m' -> m' ptr

instance MonadFail Hdf5M where
  fail a = Hdf5M $ const (fail a)

instance MonadIO Hdf5M where
  liftIO io = Hdf5M $ const $ liftIO io

instance MonadThrow Hdf5M where
  throwM e = Hdf5M $ const $ throwM e

scopeHdfFinalizers :: Hdf5M a -> Hdf5M a
scopeHdfFinalizers (Hdf5M m)
  = Hdf5M
  $ \ptr -> ContT
  $ \cnt -> runContT (m ptr) (pure . pure) >>= \case
              Left  e -> pure $ Left e
              Right a -> cnt a

askPErr :: Hdf5M (Ptr HID)
askPErr = Hdf5M $ \p_err -> pure p_err

liftBracket :: (forall r. (a -> IO r) -> IO r) -> Hdf5M a
liftBracket cnt = Hdf5M $ \_ -> ContT cnt
