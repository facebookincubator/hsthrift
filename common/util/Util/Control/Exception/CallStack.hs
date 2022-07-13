{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Adds support to work with exceptions that pack a rendered call stack.
module Util.Control.Exception.CallStack (
  throwIO, throwSTM,
) where

import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import Data.Text (Text, pack)
import GHC.Stack (
  HasCallStack,
  callStack,
  currentCallStack,
  prettyCallStack,
  renderStack,
  withFrozenCallStack,
 )

type CallStack = Text

throwIO :: (E.Exception e, HasCallStack) => (CallStack -> e) -> IO a
throwIO mkException = withFrozenCallStack $ do
  ccs <- currentCallStack
  let stack
        | null ccs = prettyCallStack callStack
        | otherwise = renderStack ccs
  E.throw $ mkException $ pack stack

throwSTM :: (E.Exception e, HasCallStack) => (CallStack -> e) -> STM a
throwSTM mkException = withFrozenCallStack $ do
  let stack = prettyCallStack callStack
  STM.throwSTM $ mkException $ pack stack
