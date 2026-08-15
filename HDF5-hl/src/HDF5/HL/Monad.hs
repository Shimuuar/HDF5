-- |
module HDF5.HL.Monad
  ( Hdf5M
  , runHdf5M
  , runLiftHdf5M
  , runHdf5MEither
  , scopeHdfFinalizers
  , throwHdf5
  , askPErr
  , liftBracket
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont hiding (cont)
import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.Marshal

import HDF5.C.Types
import HDF5.HL.Internal.ErrorTy

-- | Monad for working with HDF5 code
newtype Hdf5M s a = Hdf5M (forall r. Ptr HID -> ContT (Either Error r) IO a)
  deriving stock Functor

runHdf5M :: (forall s. Hdf5M s a) -> IO a
runHdf5M m = either throwM pure =<< runHdf5MEither m

runLiftHdf5M :: (MonadIO m, MonadThrow m) => (forall s. Hdf5M s a) -> m a
runLiftHdf5M m = either throwM pure =<< liftIO (runHdf5MEither m)

runHdf5MEither :: (forall s. Hdf5M s a) -> IO (Either Error a)
runHdf5MEither (Hdf5M action) = alloca $ \ptr -> runContT (action ptr) (pure . Right)

throwHdf5 :: Error -> Hdf5M s a
throwHdf5 e = Hdf5M $ \_ -> ContT $ \_ -> pure (Left e)

instance Applicative (Hdf5M s) where
  pure a = Hdf5M (const (pure a))
  Hdf5M f <*> Hdf5M a = Hdf5M $ \ptr -> f ptr <*> a ptr
  liftA2 f (Hdf5M a) (Hdf5M b) = Hdf5M $ \ptr -> liftA2 f (a ptr) (b ptr)

instance Monad (Hdf5M s) where
  Hdf5M m >>= f = Hdf5M $ \ptr -> do
    a <- m ptr
    case f a of Hdf5M m' -> m' ptr

instance MonadFail (Hdf5M s) where
  fail a = Hdf5M $ const (fail a)

instance MonadIO (Hdf5M s) where
  liftIO io = Hdf5M $ const $ liftIO io

instance MonadThrow (Hdf5M s) where
  throwM e = Hdf5M $ const $ throwM e

scopeHdfFinalizers :: (forall s'. Hdf5M s' a) -> Hdf5M s a
scopeHdfFinalizers (Hdf5M m)
  = Hdf5M
  $ \ptr -> ContT
  $ \cnt -> runContT (m ptr) (pure . pure) >>= \case
              Left  e -> pure $ Left e
              Right a -> cnt a

askPErr :: Hdf5M s (Ptr HID)
askPErr = Hdf5M $ \p_err -> pure p_err

liftBracket :: (forall r. (a -> IO r) -> IO r) -> Hdf5M s a
liftBracket cnt = Hdf5M $ \_ -> ContT cnt
