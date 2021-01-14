-----------------------------------------------------------------
-- Autogenerated by Thrift
--
-- DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
--  @generated
-----------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns#-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns#-}
{-# LANGUAGE GADTs #-}
module Service.Y.Service
       (YCommand(..), reqName', reqParser', respWriter', onewayFunctions')
       where
import qualified Control.Exception as Exception
import qualified Control.Monad.ST.Trans as ST
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.ByteString.Builder as Builder
import qualified Data.Default as Default
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Int as Int
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Prelude as Prelude
import qualified Service.Types as Types
import qualified Service.X.Service as X
import qualified Thrift.Binary.Parser as Parser
import qualified Thrift.Codegen as Thrift
import qualified Thrift.Processor as Thrift
import qualified Thrift.Protocol.ApplicationException.Types
       as Thrift
import Control.Applicative ((<*), (*>))
import Data.Monoid ((<>))
import Prelude ((<$>), (<*>), (++), (.), (==))

data YCommand a where
  SuperX :: X.XCommand a -> YCommand a

instance Thrift.Processor YCommand where
  reqName = reqName'
  reqParser = reqParser'
  respWriter = respWriter'
  onewayFns _ = onewayFunctions'

reqName' :: YCommand a -> Text.Text
reqName' (SuperX x) = X.reqName' x

reqParser' ::
             Thrift.Protocol p =>
             Proxy.Proxy p -> Text.Text -> Parser.Parser (Thrift.Some YCommand)
reqParser' _proxy funName
  = do Thrift.Some x <- X.reqParser' _proxy funName
       Prelude.return (Thrift.Some (SuperX x))

respWriter' ::
              Thrift.Protocol p =>
              Proxy.Proxy p ->
                Int.Int32 ->
                  YCommand a ->
                    Prelude.Either Exception.SomeException a ->
                      (Builder.Builder,
                       Prelude.Maybe (Exception.SomeException, Thrift.Blame))
respWriter' _proxy _seqNum (SuperX _x) _r
  = X.respWriter' _proxy _seqNum _x _r

onewayFunctions' :: [Text.Text]
onewayFunctions' = [] ++ X.onewayFunctions'