-- Copyright (c) Facebook, Inc. and its affiliates.

module FilePathTest (main) where

import Data.Text (Text)

import Test.HUnit
import TestRunner

import Util.FilePath

dirEnv :: DirEnv
dirEnv = DirEnv{ homeDir = "/home/sweethome/", currentDir = "/herp" }

homeDirTest :: Test
homeDirTest = TestLabel "~/ handled" . TestCase $ do
  assertEqual "raw ~"
    "/home/sweethome/"
    (absolutiseWith dirEnv "~")
  assertEqual "~/derp"
    "/home/sweethome/derp"
    (absolutiseWith dirEnv "~/derp")
  assertEqual "relative path with ~"
    "/home/sweethome/../derp"
    (absolutiseWith dirEnv "~/../derp")

relPathTest :: Test
relPathTest = TestLabel "relative path handled" . TestCase $ do
  assertEqual "Relative path"
    "/herp/derp"
    (absolutiseWith dirEnv "derp")
  assertEqual "Relative path"
    "/herp/derp"
    (absolutiseWith dirEnv ".//derp")
  assertEqual "Relative path"
    "/herp/../derp"
    (absolutiseWith dirEnv "../derp")

testPathToMName :: Test
testPathToMName = TestList
  [ mkPathToModuleTestCase
      "Basic"
      "Foo/Bar/Baz/Quux.hs"
      "Foo.Bar.Baz.Quux"
  , mkPathToModuleTestCase
      "Rooted"
      "sigma/repo/Foo/Bar/Baz.hs"
      "Foo.Bar.Baz"
  , mkPathToModuleTestCase
      "Deeply rooted test"
      "sigma/repo/Foo.hs"
      "Foo"
  , mkPathToModuleTestCase
      "Absolute Filename"
      "/data/users/gnickstanley/si_sigma/Contexts/TestContext.hs"
      "Contexts.TestContext"
  , mkPathToModuleTestCase
      "Subproject Filename"
      "duckling/Duckling/Ranking/Types.hs"
      "Duckling.Ranking.Types"
  , mkPathToModuleTestCase
      "Rooted Subproject"
      "si_sigma/duckling/Duckling/Ranking/Types.hs"
      "Duckling.Ranking.Types"
  , mkPathToModuleTestCase
      "Absolute Subproject"
      "/data/users/gnickstanley/si_sigma/duckling/Duckling/Ranking/Types.hs"
      "Duckling.Ranking.Types"
  , mkPathToModuleTestCase
      "Other Capitalized Directories"
      "/data/users/ILOVECAPITALIZEDUSERNAMES/si_sigma/Contexts/TestContext.hs"
      "Contexts.TestContext"
  ]

mkPathToModuleTestCase :: String -> FilePath -> Text -> Test
mkPathToModuleTestCase name path expected =
  TestLabel name . TestCase $ expected @=? pathToMName path

testMNameToPath :: Test
testMNameToPath = TestList
  [ mkMNameToPathTestCase
      "Basic"
      "Foo.Bar.Baz.Quux"
      "Foo/Bar/Baz/Quux.hs"
  ]

mkMNameToPathTestCase :: String -> Text -> FilePath -> Test
mkMNameToPathTestCase name mname expected =
  TestLabel name . TestCase $ expected @=? mnameToPath mname

main :: IO ()
main = testRunner $ TestList
  [ homeDirTest
  , relPathTest
  , testPathToMName
  , testMNameToPath
  ]
