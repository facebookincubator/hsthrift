-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Testing
  ( assertProperty
  , skip
  , skipTest
  , skipTestIf
  , skipTestIfRtsIsProfiled
  , skipIf
  ) where

import Control.Exception
import GHC.Stack (HasCallStack)
import System.IO
import Test.HUnit
import Test.HUnit.Lang (HUnitFailure)
import Util.Control.Exception
import qualified Test.QuickCheck as QC

import DynFlags (rtsIsProfiled)

skip :: String -> IO ()
skip msg = hPutStr stderr $ unlines [msg, "***SKIP***"]

skipIf :: (SomeException -> Bool) -> IO () -> IO ()
skipIf f = handleAll $ \e -> if f e then skip (show e) else throw e

skipTestIf :: (SomeException -> Bool) -> Test -> Test
skipTestIf f (TestCase tc) = TestCase $ skipIf f tc
skipTestIf f (TestList ts) = TestList $ map (skipTestIf f) ts
skipTestIf f (TestLabel l t) = TestLabel l $ skipTestIf f t

skipTest :: Test -> Test
skipTest = skipTestIf $ const True

skipTestIfRtsIsProfiled :: Test -> Test
skipTestIfRtsIsProfiled = skipTestIf $ const rtsIsProfiled

assertProperty
  :: (HasCallStack, QC.Testable prop) => String -> prop -> Assertion
assertProperty msg prop = do
  result <- QC.quickCheckResult prop
  case result of
    QC.Success{} -> return ()
    QC.Failure{theException = Just e}
      | Just (he :: HUnitFailure) <- fromException e -> throwIO he
    _ -> assertFailure msg
