-- Copyright (c) Facebook, Inc. and its affiliates.

module HsStructTest (main) where

import Test.HUnit
import TestRunner


import Control.Exception
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Foreign
import Foreign.C.Types (CBool(..), CChar)

import Foreign.CPP.HsStruct
import Foreign.CPP.Marshallable.TH

foreign import ccall unsafe "checkHsText"
  c_checkHsText :: Ptr HsText -> Ptr CChar -> Word -> IO CBool

pokeHsTextTest :: Test
pokeHsTextTest = TestLabel "pokeHsTextTest" $ TestCase $ do
  let
    ctorString = "constructed string"
    pokeString = "poked string"
  withCxxObject (HsText ctorString) $ \p -> do
    check_1 <- unsafeUseAsCStringLen (Text.encodeUtf8 ctorString) $
      \(str, len) -> c_checkHsText p str (fromIntegral len)
    assertBool "constructed string not matches" (toBool check_1)

    assign p (HsText pokeString)
    check_2 <- unsafeUseAsCStringLen (Text.encodeUtf8 pokeString) $
      \(str,len) -> c_checkHsText p str (fromIntegral len)
    assertBool "poked string not matches" (toBool check_2)

maybeTest :: Test
maybeTest = TestLabel "Maybe" $ TestCase $ do
  (nothing :: Maybe String) <- coerce $ peek =<< getNothing
  assertEqual "Nothing" Nothing nothing
  (just :: Maybe String) <- coerce $ peek =<< getJust
  assertEqual "Just Text" (Just "just") just

foreign import ccall unsafe "getNothing"
  getNothing :: IO (Ptr (HsMaybe HsString))

foreign import ccall unsafe "getJust"
  getJust :: IO (Ptr (HsMaybe HsString))

peekHsEitherTest :: Test
peekHsEitherTest = TestLabel "peekHsEitherTest" $ TestCase $ do
  (left :: Either String Int) <- coerce $ peek =<< getLeft
  assertEqual "Left" (Left "error") left
  (right :: Either String Int) <- coerce $ peek =<< getRight
  assertEqual "Right" (Right 42) right

foreign import ccall unsafe "getLeft"
  getLeft :: IO (Ptr (HsEither HsString Int))

foreign import ccall unsafe "getRight"
  getRight :: IO (Ptr (HsEither HsString Int))

pokeHsEitherTest :: Test
pokeHsEitherTest = TestLabel "pokeHsEitherTest" $ TestCase $ do
  let
    initialValue :: HsEither HsText Int
    initialValue = HsEither (Left (HsText "string from haskell"))
    updatedValue :: HsEither HsText Int
    updatedValue = HsEither (Right 42)
  withCxxObject initialValue $ \p -> do
    check_1 <- c_checkHsEither p 1
    assertBool "Left value not matches" (toBool check_1)
    assign p updatedValue
    check_2 <- c_checkHsEither p 2
    assertBool "Right value not matches" (toBool check_2)

foreign import ccall  unsafe "checkHsEither"
  c_checkHsEither :: Ptr (HsEither HsText Int) -> Int -> IO CBool

arrayTest :: Test
arrayTest = TestLabel "Array" $ TestCase $ do
  s <- fmap (fmap hsString . hsArray) $ peek =<< getArray
  assertEqual "Vector String" (Vector.fromList ["foo", "bar"]) s
  bs <- fmap (map hsByteString . hsList) $ peek =<< getArray
  assertEqual "[ByteString]" ["foo", "bar"] bs
  t <- fmap (map hsText . hsList) $ peek =<< getArray
  assertEqual "[Text]" ["foo", "bar"] t

foreign import ccall unsafe "getArray"
  getArray :: IO (Ptr a)

mapTest :: Test
mapTest = TestLabel "Map" $ TestCase $ do
  HsMap m <- peek =<< getIntMap
  assertEqual "Map Int Int" (Map.fromList assocs) m
  HsIntMap im <- peek =<< getIntMap
  assertEqual "IntMap Int" (IntMap.fromList assocs) im
  HsHashMap hm <- peek =<< getIntMap
  assertEqual "HashMap Int Int" (HashMap.fromList assocs) hm
  where
  assocs = [(2, 4), (3, 9), (5, 25), (7, 49)] :: [(Int, Int)]

foreign import ccall unsafe "getIntMap"
  getIntMap :: IO (Ptr a)

pairTest :: Test
pairTest = TestLabel "Pair" $ TestCase $ do
  HsPair (HsText f, s) :: HsPair HsText Int <- peek =<< getPair
  assertEqual "Pair" ("foo", 3) (f,s)

foreign import ccall unsafe "getPair"
  getPair :: IO (Ptr a)

type Nested = HashMap Text (IntMap [Maybe Text])
type HsNested = HsObject (HsIntMap (HsList (HsMaybe HsText)))

nestedTest :: Test
nestedTest = TestLabel "nested" $ TestCase $ do
  actual :: Nested <- coerce $ bracket createNested destroyNested peek
  assertEqual "Nested" expected actual
  where
  expected = HashMap.fromList
    [ ("zero", IntMap.empty)
    , ("one", IntMap.singleton 1 [])
    , ("two", IntMap.singleton 2 [Nothing])
    , ("more", IntMap.fromList
        [ (3, [Nothing])
        , (4, [Just "two"])
        , (5, [Nothing, Just ""])
        , (6, [Just "two", Just "three"])
        ]
      )
    ]

foreign import ccall unsafe "createNested"
  createNested :: IO (Ptr HsNested)

foreign import ccall unsafe "destroyNested"
  destroyNested :: Ptr HsNested -> IO ()

main :: IO ()
main = testRunner $ TestList
  [ pokeHsTextTest
  , maybeTest
  , peekHsEitherTest
  , pokeHsEitherTest
  , arrayTest
  , mapTest
  , pairTest
  , nestedTest
  ]
