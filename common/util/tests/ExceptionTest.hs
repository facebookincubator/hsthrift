-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module ExceptionTest (main) where

import Control.Exception
import Util.Control.Exception
import Test.HUnit
import TestRunner

catchAllTest :: Test
catchAllTest = TestLabel "catchAll" . TestCase $ do
  r <- (throwIO ThreadKilled `catchAll` \_ -> return 1)
      `catch` \SomeException{} -> return (2::Int)
  assertEqual "catchAll1" 2 r
  r <- (throwIO (ErrorCall "x") `catchAll` \_ -> return 1)
      `catch` \SomeException{} -> return (2::Int)
  assertEqual "catchAll2" 1 r

tryAllTest :: Test
tryAllTest = TestLabel "tryAll" . TestCase $ do
  r <- try (tryAll (throwIO ThreadKilled))
  assertBool "tryAll1" $ case r of
    Left x | Just ThreadKilled{} <- fromException x -> True
    _ -> False
  r <- try (tryAll (throwIO (ErrorCall "x")))
   :: IO (Either SomeException (Either SomeException Int))
  assertBool "tryAll2" $ case r of
    Right (Left x) | Just ErrorCall{} <- fromException x -> True
    _ -> False

tryBracketTest :: Test
tryBracketTest = TestLabel "tryBracket" . TestCase $ do
  r <- tryAll $ tryBracket
     (throwIO (ErrorCall "a"))
     (\_ _ -> throwIO (ErrorCall "b"))
     (\_ -> throwIO (ErrorCall "c"))
  assertBool "tryBracket1" $ case r of
    Left e | Just (ErrorCall "a") <- fromException e -> True
    _other -> False
  r <- tryAll $ tryBracket
     (return 'a')
     (\'a' _ -> throwIO (ErrorCall "b")) -- release throws; this is what we get
     (\'a' -> throwIO (ErrorCall "c"))
  assertBool "tryBracket2" $ case r of
    Left e | Just (ErrorCall "b") <- fromException e -> True
    _other -> False
  r <- tryAll $ tryBracket
     (return 'a')
     (\'a' Left{} -> return ())
     (\'a' -> throwIO (ErrorCall "c"))
  assertBool "tryBracket3" $ case r of
    Left e | Just (ErrorCall "c") <- fromException e -> True
    _other -> False
  r <- tryAll $ tryBracket
     (return 'a')
     (\'a' Right{} -> return ())
     (\'a' -> return 'c')
  assertBool "tryBracket4" $ case r of
    Right 'c' -> True
    _other -> False

tryFinallyTest :: Test
tryFinallyTest = TestLabel "tryFinally" . TestCase $ do
  r <- tryAll $ tryFinally
     (throwIO (ErrorCall "a"))
     (\Left{} -> throwIO (ErrorCall "b"))
  assertBool "tryFinally1" $ case r of
    Left e | Just (ErrorCall "b") <- fromException e -> True
    _other -> False
  r <- tryAll $ tryFinally
     (return 'a')
     (\Right{} -> throwIO (ErrorCall "b"))
        -- release throws; this is what we get
  assertBool "tryFinally2" $ case r of
    Left e | Just (ErrorCall "b") <- fromException e -> True
    _other -> False

main :: IO ()
main = testRunner $ TestList
  [ catchAllTest
  , tryAllTest
  , tryBracketTest
  , tryFinallyTest
  ]
