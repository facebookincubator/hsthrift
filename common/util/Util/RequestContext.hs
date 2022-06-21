-- Copyright (c) Facebook, Inc. and its affiliates.
{-# LANGUAGE TemplateHaskell #-}

module Util.RequestContext (
  RequestContext,
  CRequestContextPtr,
  createRequestContext,
  saveRequestContext,
  setRequestContext,
  createShallowCopyRequestContext,
  withRequestContext,
  finalizeRequestContext,
  withShallowCopyRequestContextScopeGuard,
  RequestContextHolder(..),
  DefaultRequestContextHolder,
) where

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

createRequestContext :: IO (Ptr CRequestContextPtr) -> IO RequestContext
createRequestContext create =
  mask_ $ fmap RequestContext $ toSharedPtr =<< create

-- | 'saveRequestContext' should only be used in bound thread created by
-- 'forkOS', 'main' or @foreign export@.
saveRequestContext :: IO RequestContext
saveRequestContext = createRequestContext c_saveContext

-- | 'setRequestContext' should only be used in bound thread created by
-- 'forkOS', 'main' or @foreign export@.
setRequestContext :: RequestContext -> IO ()
setRequestContext (RequestContext rc) = withForeignPtr rc c_setContext

-- | Creates a **shallow** copy of the 'RequestContext'. This allows to
-- overwrite a specific RequestData pointer.
createShallowCopyRequestContext :: RequestContext -> IO RequestContext
createShallowCopyRequestContext (RequestContext rc) =
  withForeignPtr rc $ createRequestContext . c_createShallowCopy

withRequestContext :: RequestContext -> (Ptr CRequestContextPtr -> IO a) -> IO a
withRequestContext (RequestContext rc) = withForeignPtr rc

finalizeRequestContext :: RequestContext -> IO ()
finalizeRequestContext (RequestContext rc) = finalizeForeignPtr rc

-- | 'withShallowCopyRequestContextScopeGuard' should only be used in bound
-- thread created by 'forkOS', 'main' or '@foreign export@'. This allows to
-- overwrite a specific RequestData pointer for the scope's duration, without
-- breaking others.
withShallowCopyRequestContextScopeGuard :: IO a -> IO a
withShallowCopyRequestContextScopeGuard f = do
  rc <- saveRequestContext
  flip finally (setRequestContext rc >> finalizeRequestContext rc) $ do
    shallowCopy <- createShallowCopyRequestContext rc
    setRequestContext shallowCopy
    finalizeRequestContext shallowCopy
    f

foreign import ccall unsafe "hs_request_context_saveContext"
  c_saveContext :: IO (Ptr CRequestContextPtr)

foreign import ccall unsafe "hs_request_context_setContext"
  c_setContext :: Ptr CRequestContextPtr -> IO ()

foreign import ccall unsafe "hs_request_context_createShallowCopy"
  c_createShallowCopy :: Ptr CRequestContextPtr -> IO (Ptr CRequestContextPtr)

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
