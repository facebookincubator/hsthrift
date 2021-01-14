-- Copyright (c) Facebook, Inc. and its affiliates.

module GFlagsTest where

import Util.GFlags

import Test.Hspec
import SpecRunner

main :: IO ()
main = specRunner $ do

  around_ withFlagSaver $ do

  let key = "gflags_test_key"
  let lol = "gflags_test_lol"

  describe "getFlagValue" $ do

    context "missing key" $ do
      it "yields an exception" $ do
        getFlagValue lol `shouldReturn` Left (FlagException lol)

    context "existent key" $ do
      it "gets the value" $ do
        getFlagValue key `shouldReturn` Right ""

  describe "setFlagValue" $ do

    context "missing key" $ do
      it "yields an exception" $ do
        setFlagValue lol "foo" `shouldReturn` Left (FlagException lol)

    context "existent key" $ do
      it "sets the value" $ do
        getFlagValue key `shouldReturn` Right ""
        setFlagValue key "foo" `shouldReturn` Right ()
        getFlagValue key `shouldReturn` Right "foo"

  describe "setFlagDefault" $ do

    context "missing key" $ do
      it "yields an exception" $ do
        setFlagDefault lol "bar" `shouldReturn` Left (FlagException lol)

    context "existent key" $ do
      it "sets the default" $ do
        getFlagValue key `shouldReturn` Right ""
        setFlagDefault key "bar" `shouldReturn` Right ()
        getFlagValue key `shouldReturn` Right "bar"
        setFlagValue key "foo" `shouldReturn` Right ()
        getFlagValue key `shouldReturn` Right "foo"
        setFlagDefault key "bar" `shouldReturn` Right ()
        getFlagValue key `shouldReturn` Right "foo"

  describe "setFlagValueIfDefault" $ do

    context "missing key" $ do
      it "yields an exception" $ do
        setFlagValueIfDefault lol "foo" `shouldReturn` Left (FlagException lol)

    context "existent key" $ do

      context "when unset" $ do
        it "sets the value" $ do
          getFlagValue key `shouldReturn` Right ""
          setFlagValueIfDefault key "foo" `shouldReturn` Right ()
          getFlagValue key `shouldReturn` Right "foo"

      context "when set" $ do
        it "does nothing" $ do
          getFlagValue key `shouldReturn` Right ""
          setFlagValue key "foo" `shouldReturn` Right ()
          setFlagValueIfDefault key "bar" `shouldReturn` Right ()
          getFlagValue key `shouldReturn` Right "foo"
