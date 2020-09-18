module Main where

import TestRunner
import Test.HUnit

import qualified HandlerTest
import qualified ServerTest

main :: IO ()
main = testRunner $ TestLabel "thrift-server-tests" $ TestList
  [ HandlerTest.tests
  , ServerTest.tests
  ]
