{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module EchoHandler
  ( echoHandler

  , EchoerState
  , initEchoerState
  ) where

import CalculatorHandler

import Echoer.Echoer.Service

newtype EchoerState = EchoerState CalculatorState

initEchoerState :: IO EchoerState
initEchoerState = EchoerState <$> initCalcState

echoHandler :: EchoerState -> EchoerCommand a -> IO a
echoHandler _ (Echo input) = return input
echoHandler (EchoerState c) (SuperCalculator x) = calculatorHandler c x
