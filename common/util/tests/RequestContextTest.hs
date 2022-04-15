module RequestContextTest
  ( main
  ) where

import Test.HUnit
import TestRunner

import Control.Exception

import RequestContextTestHelper
import Util.RequestContext

shallowCopyRequestContextScopeGuardTest :: Test
shallowCopyRequestContextScopeGuardTest =
  TestLabel "shallowCopyRequestContextScopeGuardTest" $ TestCase $ do
    assertEqual "top:init" 0 =<< getCurrentTestValue
    setCurrentTestValue 1
    assertEqual "top:before" 1 =<< getCurrentTestValue
    withShallowCopyRequestContextScopeGuard $ do
      assertEqual "outer:init" 1 =<< getCurrentTestValue
      setCurrentTestValue 10
      assertEqual "outer:before1" 10 =<< getCurrentTestValue
      withShallowCopyRequestContextScopeGuard $ do
        setCurrentTestValue 101
        assertEqual "inner1" 101 =<< getCurrentTestValue
      assertEqual "outer:before2" 10 =<< getCurrentTestValue
      handle (\(ErrorCall "test") -> return ()) $
        withShallowCopyRequestContextScopeGuard $ do
          setCurrentTestValue 102
          assertEqual "inner2" 102 =<< getCurrentTestValue
          throwIO $ ErrorCall "test"
      assertEqual "outer:after" 10 =<< getCurrentTestValue
    assertEqual "top:after" 1 =<< getCurrentTestValue

main :: IO ()
main = testRunner $ TestList
  [ shallowCopyRequestContextScopeGuardTest
  ]
