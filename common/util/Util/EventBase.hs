-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.EventBase
  ( CppIOExecutor
  , EventBase(..)
  , EventBaseDataplane(..)
  , EventBaseProvider(..)
  , EventBaseDataplaneProvider(..)
  , withEventBaseDataplane
  , initExecutor
  , destroyExecutor
  ) where

import Control.Exception
import Foreign.Ptr
import Util.Executor

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

class EventBaseProvider a => EventBaseDataplaneProvider a where
  getEventBaseDataplane :: a -> IO EventBaseDataplane

instance EventBaseProvider EventBaseDataplane where
  getEventBase (EventBaseDataplane ptr) =
    EventBase <$> c_getIOExecutorEventBase ptr

instance EventBaseDataplaneProvider EventBaseDataplane where
  getEventBaseDataplane = return

instance ExecutorProvider EventBase where
  -- event bases are executors
  getExecutor (EventBase p) = Executor <$> c_castEventBaseToExecutor p

instance ExecutorProvider EventBaseDataplane where
  -- io executors are executors
  getExecutor (EventBaseDataplane p) = Executor <$> c_castIOExecutorToExecutor p

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

foreign import ccall unsafe "common_hs_eventbase_newExecutor"
  c_newExecutor :: IO (Ptr CppIOExecutor)

foreign import ccall unsafe "common_hs_eventbase_destroyExecutor"
  c_destroyExecutor :: Ptr CppIOExecutor -> IO ()

foreign import ccall unsafe "common_hs_eventbase_getIOExecutorEventBase"
  c_getIOExecutorEventBase :: Ptr CppIOExecutor -> IO (Ptr CppEventBase)

-- we cannot just use Haskell `castPtr` as there is virtual inheritance involved

foreign import ccall unsafe "common_hs_eventbase_castIOExecutorToExecutor"
  c_castIOExecutorToExecutor :: Ptr CppIOExecutor -> IO (Ptr CppFollyExecutor)

foreign import ccall unsafe "common_hs_eventbase_castEventBaseToExecutor"
  c_castEventBaseToExecutor :: Ptr CppEventBase -> IO (Ptr CppFollyExecutor)
