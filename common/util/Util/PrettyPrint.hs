{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.PrettyPrint
  ( renderNumWithUnit
  , renderPercent
  , renderIncrease
  , renderNum
  , renderInt
  , renderBytes
  , renderBytesString
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf
import TextShow

renderNumWithUnit :: Text -> Double -> Text
renderNumWithUnit unit num = renderNum num <> " " <> unit

-- | Render a fraction as a percentage
renderPercent :: Double -> Double -> Text
renderPercent x y
  | y == 0     = "Infinite"
  | otherwise  = Text.pack $ printf (printf "%%.%df%%%%" precision) frac
  where
    frac = x / y * 100
    -- Make sure we always have enough signifigant digits
    precision = max 0 $ ceiling (negate $ logBase 10 frac) + 2 :: Int

-- | Render an increase as either a percent or times
renderIncrease :: Double -> Double -> Text
renderIncrease x y
  | y == 0    = "Infinite"
  | frac < 2  = renderPercent (x - y) y
  | frac < 10 = Text.pack $ printf "%.2fx" frac
  | otherwise = showt (round frac :: Int) <> "x"
  where
    frac = x / y

renderNum :: Double -> Text
renderNum num
  | num < 10 = Text.pack $ printf "%.2f" num
  | otherwise = renderInt $ round num

renderInt :: Int -> Text
renderInt num
  | num < 1000 = showt num
  | num < 1000000 = showt (num `div` 1000) <> "k"
  | otherwise = showt (num `div` 1000000) <> "M"

renderBytes :: Int -> Text
renderBytes b = Text.pack $ renderBytesString b

renderBytesString :: Int -> String
renderBytesString b
 | b' > gb = printf "%.2f GiB" (b' / gb)
 | b' > mb = printf "%.2f MiB" (b' / mb)
 | b' > kb = printf "%.2f kiB" (b' / kb)
 | otherwise = printf "%d bytes" (fromIntegral b :: Integer)
 where
  kb, mb, gb :: Double
  kb = 1024
  mb = 1024*kb
  gb = 1024*mb
  b' = fromIntegral b :: Double
