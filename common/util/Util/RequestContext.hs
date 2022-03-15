-- Copyright (c) Facebook, Inc. and its affiliates.
{-# LANGUAGE TemplateHaskell #-}

module Util.RequestContext (
  RequestContext,
  CRequestContextPtr,
  saveRequestContext,
  setRequestContext,
  withRequestContext,
  finalizeRequestContext,
  forkIOWithRequestContext,
  forkOnWithRequestContext,
  RequestContextHolder(..),
  DefaultRequestContextHolder,
) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Foreign.CPP.Marshallable.TH
import Foreign.ForeignPtr
import Foreign.Ptr

data CRequestContextPtr

$(deriveDestructibleUnsafe "RequestContextPtr" [t|CRequestContextPtr|])

newtype RequestContext = RequestContext (ForeignPtr CRequestContextPtr)

instance NFData RequestContext where
  rnf (RequestContext rc) = rc `seq` ()

-- | 'saveRequestContext' should only be used in bound thread created by
-- 'forkOS', 'main' or @foreign export@.
saveRequestContext :: IO RequestContext
saveRequestContext = mask_ $ fmap RequestContext $ toSharedPtr =<< c_saveContext

-- | 'setRequestContext' should only be used in bound thread created by
-- 'forkOS', 'main' or @foreign export@.
setRequestContext :: RequestContext -> IO ()
setRequestContext (RequestContext rc) = withForeignPtr rc c_setContext

withRequestContext :: RequestContext -> (Ptr CRequestContextPtr -> IO a) -> IO a
withRequestContext (RequestContext rc) = withForeignPtr rc

finalizeRequestContext :: RequestContext -> IO ()
finalizeRequestContext (RequestContext rc) = finalizeForeignPtr rc

foreign import ccall unsafe "hs_request_context_saveContext"
  c_saveContext :: IO (Ptr CRequestContextPtr)

foreign import ccall unsafe "hs_request_context_setContext"
  c_setContext :: Ptr CRequestContextPtr -> IO ()

-- The returned 'IO ()' can only be called at most once.
restorableRequestContext :: IO (IO ())
restorableRequestContext = do
  rc <- saveRequestContext
  return $ do
    setRequestContext rc
    finalizeRequestContext rc

forkIOWithRequestContext :: IO () -> IO ThreadId
forkIOWithRequestContext f = do
  restore <- restorableRequestContext
  forkIO $ restore >> f

forkOnWithRequestContext :: Int -> IO () -> IO ThreadId
forkOnWithRequestContext cap f = do
  restore <- restorableRequestContext
  forkOn cap $ restore >> f

class RequestContextHolder a where
  trySaveRequestContextFrom :: a -> IO (Maybe RequestContext)
  trySetRequestContextTo :: Maybe RequestContext -> a -> IO a

data DefaultRequestContextHolder = DefaultRequestContextHolder
  deriving (Eq, Show)

instance RequestContextHolder DefaultRequestContextHolder where
  trySaveRequestContextFrom _ = Just <$> saveRequestContext
  trySetRequestContextTo rc a = mapM_ setRequestContext rc *> return a

instance RequestContextHolder () where
  trySaveRequestContextFrom _ = return Nothing
  trySetRequestContextTo _ = return
