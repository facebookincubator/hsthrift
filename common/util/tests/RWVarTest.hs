-- Copyright (c) Facebook, Inc. and its affiliates.

module RWVarTest (main) where

import Test.HUnit
import TestRunner

import Facebook.Init
import Util.RWVar
import Util.HUnit

readTest :: Test
readTest = TestLabel "readTest" $ TestCase $ do
  var <- newRWVar expected
  withReadRWVar var $ \v ->
    assertEqual "got expected back" expected v
  where
    expected = 'a'

writeTest :: Test
writeTest = TestLabel "writeTest" $ TestCase $ do
  var <- newRWVar initial
  withWriteRWVar var $ \v -> do
    assertEqual "got initial" initial v
    return (update, ())
  withReadRWVar var $ \v ->
    assertEqual "got updated value" update v
  where
    initial :: Int
    initial = 42
    update = 1337

readReadTest :: Test
readReadTest = TestLabel "nestedReads" $ TestCase $ do
  var <- newRWVar val
  withReadRWVar var $ \x -> do
    assertEqual "first got value" val x
    withReadRWVar var $ \y ->
      assertEqual "second got value" val y
  where
    val = 'a'

readWriteDeadlockTest :: Test
readWriteDeadlockTest = TestLabel "read then write deadlocks" $ TestCase $ do
  var <- newRWVar 'a'
  assertThrow "deadlocks" $
    withReadRWVar var $ \_ ->
      withWriteRWVar var $ \a -> return (a, ())

writeReadDeadlockTest :: Test
writeReadDeadlockTest = TestLabel "write then read deadlocks" $ TestCase $ do
  var <- newRWVar 'a'
  assertThrow "deadlocks" $
    withWriteRWVar var $ \a -> do
      _ <- withReadRWVar var return
      return (a, ())

writeWriteDeadlockTest :: Test
writeWriteDeadlockTest = TestLabel "write then write deadlocks" $ TestCase $ do
  var <- newRWVar 'a'
  assertThrow "deadlocks" $
    withWriteRWVar var $ \x -> do
      _ <- withWriteRWVar var $ \y -> return (y, ())
      return (x, ())


main :: IO ()
main = withFacebookUnitTest $
  testRunner $ TestList
   [ readTest
   , writeTest
   , readReadTest
   , readWriteDeadlockTest
   , writeReadDeadlockTest
   , writeWriteDeadlockTest
   ]
