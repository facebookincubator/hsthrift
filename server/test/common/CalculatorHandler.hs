{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module CalculatorHandler
  ( calculatorHandler
  , CalculatorState
  , initCalcState
  ) where

import Control.Exception (throw)
import Data.IORef
import Thrift.Protocol.ApplicationException.Types

import Math.Types
import Math.Adder.Service
import Math.Calculator.Service

newtype CalculatorState = CalculatorState (IORef Int)

initCalcState :: IO CalculatorState
initCalcState = do
  r <- newIORef 0
  return $ CalculatorState r

calculatorHandler :: CalculatorState -> CalculatorCommand a -> IO a
calculatorHandler _ (SuperAdder (Add x y)) = return $ x + y
calculatorHandler _ (Divide x y)
  | y == 0    = throw DivideByZero
  | otherwise = return $ x / y
calculatorHandler _ (QuotRem x y) =
  -- Intentionally leak an unhandled error
  return $ QuotRemResponse (x `quot` y) (x `rem` y)
calculatorHandler (CalculatorState ref) (Put v) =
  writeIORef ref (fromIntegral v)
calculatorHandler (CalculatorState ref) (PutMany v) =
  mapM_ (writeIORef ref . fromIntegral) v
calculatorHandler (CalculatorState ref) Get =
  fromIntegral <$> readIORef ref
calculatorHandler _ Unimplemented =
  throw $ ApplicationException "Unimplemented function"
  ApplicationExceptionType_UnknownMethod
