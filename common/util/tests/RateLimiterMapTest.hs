-- Copyright (c) Facebook, Inc. and its affiliates.

module RateLimiterMapTest where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, copy)

import Data.RateLimiterMap

import Test.HUnit
import TestRunner

rlTest :: Assertion
rlTest = do
  let reduceM z xs f = foldM f z xs

  crl <- newRateLimiterMapWithKeyPreprocessor 1 1 (return . copy)
  -- 500 reqs to "foo" with 10 millisecond delay (100 qps for 5 secs)
  (fooCount, _) <-
    reduceM (0::Int, 0) [1..500] $ \ acc@(c,w) i -> do
      threadDelay (10^(4::Int)) -- 10 milliseconds
      allowed <- isAllowed crl "foo"
      case allowed of
        NotAllowed -> return acc
        Allowed sw -> do
          assertEqual "weight sum counts all" i (sw+w)
          return (c+1, sw+w)
  assertBool "was actually allowed" (fooCount > 0)
  assertBool "was actually limited" (fooCount < 20)

rlThreadedTest :: Assertion
rlThreadedTest = do
  crl <- newRateLimiterMap 5 5 :: IO (RateLimiterMap Text)
  mv <- newMVar (mempty :: HashMap Text (Int, SampleWeight))
  -- 50k reqs each to "foo" and "bar" from 100k threads,
  -- requests come in bursts every quarter second for 5 secs
  -- a test with 100k threads? sure why not
  forConcurrently_ (zip [1..100000] $ cycle ["foo", "bar"]) $ \ (i, nm) -> do
    threadDelay ((i `mod` 20) * 25 * 10^(4::Int))
    allowed <- isAllowed crl nm
    case allowed of
      NotAllowed -> return ()
      Allowed sw ->
        modifyMVar_ mv $
          return . HashMap.insertWith (\(c',w') (c,w) -> (c+c', w+w')) nm (1,sw)
  m <- takeMVar mv
  let (fooCount,_) = HashMap.lookupDefault (0,0) "foo" m
      (barCount,_) = HashMap.lookupDefault (0,0) "bar" m
  assertBool "foo was allowed" (fooCount > 0)
  assertBool "bar was allowed" (barCount > 0)
  assertBool "foo was limited" (fooCount < 100)
  assertBool "bar was limited" (barCount < 100)

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "RateLimiterMap" $ TestCase rlTest
  , TestLabel "RateLimiterMapThreads" $ TestCase rlThreadedTest
  ]
