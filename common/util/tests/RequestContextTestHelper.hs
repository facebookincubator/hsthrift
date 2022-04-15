module RequestContextTestHelper
  ( getCurrentTestValue
  , setCurrentTestValue
  , getTestValue
  , setTestValue
  ) where

import Control.Exception
import Foreign

import Util.RequestContext

getCurrentTestValue :: IO Int
getCurrentTestValue =
  bracket saveRequestContext finalizeRequestContext getTestValue

setCurrentTestValue :: Int -> IO ()
setCurrentTestValue value =
  bracket saveRequestContext finalizeRequestContext $ \rc -> do
    setTestValue rc value
    setRequestContext rc

getTestValue :: RequestContext -> IO Int
getTestValue rc = withRequestContext rc c_getTestValue

setTestValue :: RequestContext -> Int -> IO ()
setTestValue rc value = withRequestContext rc $ flip c_setTestValue value

foreign import ccall unsafe "hs_request_context_getTestValue"
  c_getTestValue :: Ptr CRequestContextPtr -> IO Int

foreign import ccall unsafe "hs_request_context_setTestValue"
  c_setTestValue :: Ptr CRequestContextPtr -> Int -> IO ()
