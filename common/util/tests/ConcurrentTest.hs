-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ConcurrentTest where

import Control.Exception
import Data.IORef

import Test.HUnit
import TestRunner

import Util.Concurrent

cacheSuccessTest :: Test
cacheSuccessTest = TestLabel "cacheSuccess" $ TestCase $ do
  c <- cacheSuccess (return (3::Int))
  r <- c
  assertEqual "cacheSuccess ok" r 3
  r <- c
  assertEqual "cacheSuccess ok2" r 3
  x <- newIORef (0::Int)
  c <- cacheSuccess (modifyIORef x (+1) >> readIORef x)
  r <- c
  assertEqual "cacheSuccess ok3" r 1
  r <- c
  assertEqual "cacheSuccess ok4" r 1
  x <- newIORef (0::Int)
  c <- cacheSuccess $ do
    modifyIORef x (+1); z <- readIORef x; throwIO $ ErrorCall $ show z
  r <- try c :: IO (Either ErrorCall ())
  assertEqual "cacheSuccess fail1" r (Left (ErrorCall "1"))
  r <- try c :: IO (Either ErrorCall ())
  assertEqual "cacheSuccess fail2" r (Left (ErrorCall "2"))

main :: IO ()
main = testRunner $ TestList
  [ cacheSuccessTest
  ]
