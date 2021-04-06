-- Copyright (c) Facebook, Inc. and its affiliates.

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
