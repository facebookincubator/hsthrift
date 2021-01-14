-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Fd
  ( withFdEventNotification
  ) where

import Control.Exception
import Control.Monad
import GHC.Event hiding (closeFd)
import Foreign.C
import System.Posix.IO
import System.Posix.Types

-- | Uses a file descriptor and GHC's event manager to run a callback
-- once the file descriptor is written to. Is less expensive than a new FFI
-- call from C++ back into Haskell.
withFdEventNotification
  :: Exception e
  => e -- ^ Exception to throw when unable to get the event manager
  -> IO () -- ^ The callback to run on fd write
  -> Lifetime -- ^ OneShot or MultiShot
  -> (Fd -> IO a) -- ^ Action to run with the file descriptor to write to
  -> IO a
withFdEventNotification err callback lifetime action = do
  evm <- maybe (throw err) return =<< getSystemEventManager
  withEventFd $ \fd ->
    bracket (registerFd evm cb fd evtRead lifetime) (unregisterFd evm) $
      const $ action fd
  where
    cb _ _ = callback

withEventFd :: (Fd -> IO a) -> IO a
withEventFd = bracket
  (do fd <- c_eventfd 0 0
      when (fd == -1) $ throwErrno "eventFd"
      return $ Fd fd)
  closeFd

foreign import ccall unsafe "eventfd"
  c_eventfd :: CInt -> CInt -> IO CInt
