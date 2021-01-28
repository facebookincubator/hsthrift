-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -fno-warn-orphans #-}
module BufferTest (main) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Word (Word8)
import Foreign.Marshal.Utils
import Foreign.Ptr (castPtr)
import Test.HUnit
import Test.QuickCheck

import Facebook.Init
import TestRunner
import Util.Testing

import qualified Util.Buffer as Buffer

data FillStep
  = FillByte Word8
  | FillByteString ByteString
  | FillAlloc Int ByteString
  deriving(Show)

stepToBS :: FillStep -> ByteString
stepToBS (FillByte x) = BS.singleton x
stepToBS (FillByteString x) = x
stepToBS (FillAlloc _ x) = x

step :: FillStep -> Buffer.Fill s ()
step (FillByte x) = Buffer.byte x
step (FillByteString x) = Buffer.byteString x
step (FillAlloc n x) = Buffer.alloc n $ \p -> unsafeIOToST $
  BS.unsafeUseAsCStringLen x $ \(q,k) -> do
    copyBytes p (castPtr q) k
    return k

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

instance Arbitrary FillStep where
  arbitrary = oneof
    [ FillByte <$> arbitrary
    , FillByteString <$> arbitrary
    , (\(NonNegative n) s -> FillAlloc (BS.length s + n) s)
        <$> arbitrary
        <*> arbitrary
    ]

prop_fillByteString :: NonEmptyList (NonEmptyList FillStep) -> Property
prop_fillByteString ss =
  runST (Buffer.fillByteString 1
    $ foldr1 (\p q -> p >>= \_ -> q)
    $ map (foldr1 (>>) . map step) stepss
    )
  ===
  mconcat (map stepToBS $ concat stepss)
  where
    stepss = getNonEmpty <$> getNonEmpty ss

main :: IO ()
main = withFacebookUnitTest $ testRunner $ TestList
  [ TestLabel "fillByteString" $ TestCase $ assertProperty "mismatch"
      prop_fillByteString
  ]
