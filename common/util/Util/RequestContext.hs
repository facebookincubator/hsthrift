-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}
module Util.RequestContext
  ( RequestContext
  , saveRequestContext
  , setRequestContext
  , forkIOWithRequestContext
  , forkOnWithRequestContext
  ) where

import Control.Concurrent
import Control.Exception
import Foreign.Ptr
import Foreign.ForeignPtr

import Foreign.CPP.Marshallable.TH

data CRequestContextPtr

$(deriveDestructibleUnsafe "RequestContextPtr" [t| CRequestContextPtr |])

type RequestContext = ForeignPtr CRequestContextPtr

saveRequestContext :: IO RequestContext
saveRequestContext = mask_ $ toSharedPtr =<< c_saveContext

setRequestContext :: RequestContext -> IO ()
setRequestContext = flip withForeignPtr c_setContext

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
    finalizeForeignPtr rc

forkIOWithRequestContext :: IO () -> IO ThreadId
forkIOWithRequestContext f = do
  restore <- restorableRequestContext
  forkIO $ restore >> f

forkOnWithRequestContext :: Int -> IO () -> IO ThreadId
forkOnWithRequestContext cap f = do
  restore <- restorableRequestContext
  forkOn cap $ restore >> f
