-- Copyright (c) Facebook, Inc. and its affiliates.

module RequiredSymbolsTest where

import Data.Int
import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol
import qualified Data.Map.Strict as Map

import Huge.Types
import Huge.Service.Client

-- This stuff is defined in huge.thrift, but we don't require it so it shouldn't
-- already be in scope
data A
data B
data G
data H
data I
data J
data K
data L
data M
data N
data O
data P
data Q
data R
data S
data T
data U
data V

weDontNeedThis :: ()
weDontNeedThis = ()

-- All this stuff should be in scope
cThing :: C
cThing = C (D F_F) (E foo)

xThing :: X
xThing = Z $ Map.fromList [(Y_Y, Z Map.empty)]

functionThing :: ()
functionThing = ()
  where _xx :: (Protocol p, ClientChannel c) => Int64 -> ThriftM p c Service ()
        _xx = weNeedThis

didTheHsIncludeThingMakeIt :: HsInclude
didTheHsIncludeThingMakeIt = HsInclude

main :: IO ()
main = cThing `seq` xThing `seq`
       functionThing `seq` weDontNeedThis `seq` return ()
