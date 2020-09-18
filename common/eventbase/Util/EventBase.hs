module Util.EventBase
  ( CppIOExecutor
  , EventBase(..)
  , EventBaseDataplane(..)
  , EventBaseProvider(..)
  , withEventBaseDataplane
  , initExecutor
  , destroyExecutor
  ) where

import Control.Exception
import Foreign.Ptr

--
--  Makes an IOExecutor and makes it available in Haskell.
--

--
--  export
--

newtype EventBase = EventBase (Ptr CppEventBase)

class EventBaseProvider a where
  getEventBase :: a -> IO EventBase

newtype EventBaseDataplane = EventBaseDataplane (Ptr CppIOExecutor)

instance EventBaseProvider EventBaseDataplane where
  getEventBase (EventBaseDataplane ptr) =
    EventBase <$> c_getExecutorEventBase ptr

withEventBaseDataplane
    :: (EventBaseDataplane -> IO b)
    -> IO b
withEventBaseDataplane = bracket initExecutor destroyExecutor

initExecutor :: IO EventBaseDataplane
initExecutor = EventBaseDataplane <$> c_newExecutor

destroyExecutor :: EventBaseDataplane -> IO ()
destroyExecutor (EventBaseDataplane ptr) = c_destroyExecutor ptr

--
--  foreign
--

data CppEventBase
data CppIOExecutor

foreign import ccall unsafe "newExecutor"
  c_newExecutor :: IO (Ptr CppIOExecutor)

foreign import ccall unsafe "destroyExecutor"
  c_destroyExecutor :: Ptr CppIOExecutor -> IO ()

foreign import ccall unsafe "getExecutorEventBase"
  c_getExecutorEventBase :: Ptr CppIOExecutor -> IO (Ptr CppEventBase)
