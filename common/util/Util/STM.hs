-- Copyright (c) Facebook, Inc. and its affiliates.

-- | Reexports 'Control.Concurrent.STM'
--   adding logging for deadlocks and utility functions
module Util.STM
 ( updateTVar
 , atomicallyWithLabel
 , Util.STM.atomically
 , module Control.Concurrent.STM
 ) where

import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM hiding (atomically)
import Control.Exception (Exception, BlockedIndefinitelyOnSTM, catch, throwIO)
import GHC.Stack
import Data.Typeable (Typeable)

-- | version of @modifyTVar'@ that returns the new value
updateTVar :: TVar a -> (a -> a) -> STM a
updateTVar var f = do
  x <- readTVar var
  let !x' = f x
  writeTVar var x'
  return x'

-- | Version of 'atomically' that takes a label
atomicallyWithLabel :: String -> STM a -> IO a
atomicallyWithLabel label stm = do
  STM.atomically stm `catch` (\(_::BlockedIndefinitelyOnSTM) ->
    throwIO (BlockedIndefinitelyOnNamedSTM label))

-- | Version of 'atomically' that labels the transaction using the call site
atomically :: HasCallStack => STM a -> IO a
atomically =
  atomicallyWithLabel (getCaller $ getCallStack callStack)

-- | The calling point is actually right next to the head as the head is the
-- current function (in this case it would be logInfo/Warning/Error/Fatal).
getCaller :: [(String, SrcLoc)] -> String
getCaller cs =
  case cs of
    -- Take the first element of the stack if it exists
    ((_,sl):_) -> prettySrcLoc sl
    _       -> "Unknown stack trace"

-- | 'atomicallyWithLabel' replaces occurrences of 'BlockedIndefinitelyOnSTM' w/
-- 'BlockedIndefinitelyOnNamedSTM', carrying the name of the transaction and
-- thus giving more helpful error messages.
newtype BlockedIndefinitelyOnNamedSTM = BlockedIndefinitelyOnNamedSTM String
    deriving (Typeable)

instance Show BlockedIndefinitelyOnNamedSTM where
    showsPrec _ (BlockedIndefinitelyOnNamedSTM name) =
        showString $ "thread blocked indefinitely in STM transaction: " ++ name

instance Exception BlockedIndefinitelyOnNamedSTM
