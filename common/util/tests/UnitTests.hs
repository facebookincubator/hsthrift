-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module UnitTests (main) where

import Test.HUnit
import TestRunner

import Util.ByteString
import Util.Aeson
import qualified Util.Text as UT
import Data.Aeson.Types
import Data.Aeson (decode)
import Data.ByteString (packCString, packCStringLen, ByteString)
import Data.Complex
import qualified Data.Vector as V
import Numeric.FFT.Vector.Unitary as FFTW

import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr (nullPtr, Ptr)

import TextShow

intToByteStringTest :: Test
intToByteStringTest = TestCase $ do
  assertEqual "min int" (intToByteString minBound) "-9223372036854775808"
  assertEqual "max int" (intToByteString maxBound) "9223372036854775807"
  assertEqual "zero" (intToByteString 0) "0"

parseValueStrictTest :: Test
parseValueStrictTest = TestCase $ do
  assertEqual "false" (Right $ Bool False) $ parseValueStrict' "false"
  assertEqual "0.1" (Right $ Number 0.1) $ parseValueStrict' "0.1"
  assertEqual "consume all input" (Left "endOfInput") $
    parseValueStrict' "falsee"

#ifdef FACEBOOK
-- | We've patched the official aeson library to handle NaN. See #5183005
-- This test ensures that we notice if we forget to patch on upgrade.
parseNaN :: Test
parseNaN = TestCase $
  case decode "[NaN]" of
    Just [a :: Double] ->
      assertBool "NaN" $ isNaN a
    _ -> assertFailure "couldn't parse NaN"
#endif

-- | Check that we can still call vector-fftw after T22849884
useFFTW :: Test
useFFTW = TestCase $
  assertEqual "useFFTW" [17,5,2,1,1,1,1,1,2,5] $
    applyFFT [1..10]
  where
    applyFFT :: [Int] -> [Int]
    applyFFT series =
      map (floor . magnitude)
      . V.toList
      . FFTW.execute (FFTW.plan FFTW.dft $ length series)
      . V.fromList
      $ map (doubleToComplex . fromIntegral) series
      where
        doubleToComplex :: Double -> Complex Double
        doubleToComplex d = d :+ 0.0

withCStringLenTest :: Test
withCStringLenTest = TestCase $
  UT.withCStringLen "Hello World" $ \(ptr, _) -> do
    val <- peek ptr
    assertEqual "first character" val (castCharToCChar 'H')

useByteStringsAsCStringsTest :: Test
useByteStringsAsCStringsTest = TestCase $
  useByteStringsAsCStrings ["hello", "world"] $ \ptr -> do
    hello <- packCString =<< peek ptr
    world <- packCString =<< peek (advancePtr ptr 1)
    term <- peek $ advancePtr ptr 2
    assertEqual "first word" hello "hello"
    assertEqual "second word" world "world"
    assertEqual "terminator" term nullPtr

-- This should probably be a QuickCheck Property
bsListAsCStrLenArrTest :: [ByteString] -> Test
bsListAsCStrLenArrTest bs = TestCase $
  bsListAsCStrLenArr bs $ \strPtr lenPtr len -> do
    let packBS :: CSize -> IO ByteString
        packBS i = packCStringLen =<< (,)
                   <$> peek (getPtr strPtr)
                   <*> (fromIntegral <$> peek (getPtr lenPtr))
            where getPtr :: Storable a => Ptr a -> Ptr a
                  getPtr = (`advancePtr` fromIntegral i)
    got <- mapM packBS [0..len - 1]
    assertEqual "same list" got bs

data MyEnum = MyA | MyB
  deriving (Bounded, Enum, Eq, Show)

instance UT.TextRead MyEnum
instance TextShow MyEnum where
  showb MyA = "MyA"
  showb MyB = "MyB"

readTextTest :: Test
readTextTest = TestCase $ do
  assertEqual "myA" (Just MyA) (UT.readText "MyA")
  assertEqual "Nothing" (Nothing :: Maybe MyEnum) (UT.readText "MyC")

insertCommasAndAndTest :: Test
insertCommasAndAndTest = TestCase $ do
  assertEqual "null" "" $ UT.insertCommasAndAnd []
  assertEqual "singleton" "foo" $ UT.insertCommasAndAnd ["foo"]
  assertEqual "pair" "foo and bar" $ UT.insertCommasAndAnd ["foo", "bar"]
  assertEqual "triple" "foo, bar, and baz" $
    UT.insertCommasAndAnd ["foo", "bar", "baz"]
  assertEqual "quad" "foo, bar, baz, and quux" $
    UT.insertCommasAndAnd ["foo", "bar", "baz", "quux"]
  assertEqual "10" "1, 2, 3, 4, 5, 6, 7, 8, 9, and 10" $
    UT.insertCommasAndAnd [ showt (i::Int) | i <- [1..10] ]

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "intToByteString" intToByteStringTest
  , TestLabel "parseValueStrictTest" parseValueStrictTest
#ifdef FACEBOOK
  , TestLabel "parseNaN" parseNaN
#endif
  , TestLabel "useFFTW" useFFTW
  , TestLabel "withCStringLen" withCStringLenTest
  , TestLabel "useByteStringsAsCStrings" useByteStringsAsCStringsTest
  , TestLabel "bsListAsCStrLenArr" (bsListAsCStrLenArrTest ["hello", "world"])
  , TestLabel "readText" readTextTest
  , TestLabel "insertCommasAndAnd" insertCommasAndAndTest
  ]
