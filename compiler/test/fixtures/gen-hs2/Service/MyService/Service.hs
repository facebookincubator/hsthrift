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
module Service.MyService.Service
       (MyServiceCommand(..), reqName', reqParser', respWriter',
        onewayFunctions')
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
import qualified Thrift.Binary.Parser as Parser
import qualified Thrift.Codegen as Thrift
import qualified Thrift.Processor as Thrift
import qualified Thrift.Protocol.ApplicationException.Types
       as Thrift
import Control.Applicative ((<*), (*>))
import Data.Monoid ((<>))
import Prelude ((<$>), (<*>), (++), (.), (==))

data MyServiceCommand a where
  TestFunc :: Int.Int64 -> Types.Z -> MyServiceCommand Int.Int64
  Foo :: MyServiceCommand ()

instance Thrift.Processor MyServiceCommand where
  reqName = reqName'
  reqParser = reqParser'
  respWriter = respWriter'
  onewayFns _ = onewayFunctions'

reqName' :: MyServiceCommand a -> Text.Text
reqName' (TestFunc __field__arg1 __field__arg2) = "testFunc"
reqName' Foo = "foo"

reqParser' ::
             Thrift.Protocol p =>
             Proxy.Proxy p ->
               Text.Text -> Parser.Parser (Thrift.Some MyServiceCommand)
reqParser' _proxy "testFunc"
  = ST.runSTT
      (do Prelude.return ()
          __field__arg1 <- ST.newSTRef Default.def
          __field__arg2 <- ST.newSTRef Types.z
          let
            _parse _lastId
              = do _fieldBegin <- Trans.lift
                                    (Thrift.parseFieldBegin _proxy _lastId _idMap)
                   case _fieldBegin of
                     Thrift.FieldBegin _type _id _bool -> do case _id of
                                                               1 | _type == Thrift.getI64Type _proxy
                                                                   ->
                                                                   do !_val <- Trans.lift
                                                                                 (Thrift.parseI64
                                                                                    _proxy)
                                                                      ST.writeSTRef __field__arg1
                                                                        _val
                                                               2 | _type ==
                                                                     Thrift.getStructType _proxy
                                                                   ->
                                                                   do !_val <- Trans.lift
                                                                                 (Thrift.parseStruct
                                                                                    _proxy)
                                                                      ST.writeSTRef __field__arg2
                                                                        _val
                                                               _ -> Trans.lift
                                                                      (Thrift.parseSkip _proxy _type
                                                                         (Prelude.Just _bool))
                                                             _parse _id
                     Thrift.FieldEnd -> do !__val__arg1 <- ST.readSTRef __field__arg1
                                           !__val__arg2 <- ST.readSTRef __field__arg2
                                           Prelude.pure
                                             (Thrift.This (TestFunc __val__arg1 __val__arg2))
            _idMap = HashMap.fromList [("arg1", 1), ("arg2", 2)]
          _parse 0)
reqParser' _proxy "foo"
  = ST.runSTT
      (do Prelude.return ()
          let
            _parse _lastId
              = do _fieldBegin <- Trans.lift
                                    (Thrift.parseFieldBegin _proxy _lastId _idMap)
                   case _fieldBegin of
                     Thrift.FieldBegin _type _id _bool -> do case _id of
                                                               _ -> Trans.lift
                                                                      (Thrift.parseSkip _proxy _type
                                                                         (Prelude.Just _bool))
                                                             _parse _id
                     Thrift.FieldEnd -> do Prelude.pure (Thrift.This Foo)
            _idMap = HashMap.fromList []
          _parse 0)
reqParser' _ funName
  = Prelude.errorWithoutStackTrace
      ("unknown function call: " ++ Text.unpack funName)

respWriter' ::
              Thrift.Protocol p =>
              Proxy.Proxy p ->
                Int.Int32 ->
                  MyServiceCommand a ->
                    Prelude.Either Exception.SomeException a ->
                      (Builder.Builder,
                       Prelude.Maybe (Exception.SomeException, Thrift.Blame))
respWriter' _proxy _seqNum TestFunc{} _r
  = (Thrift.genMsgBegin _proxy "testFunc" _msgType _seqNum <>
       _msgBody
       <> Thrift.genMsgEnd _proxy,
     _msgException)
  where
    (_msgType, _msgBody, _msgException)
      = case _r of
          Prelude.Left _ex | Prelude.Just
                               _e@Thrift.ApplicationException{} <- Exception.fromException _ex
                             ->
                             (3, Thrift.buildStruct _proxy _e,
                              Prelude.Just (_ex, Thrift.ServerError))
                           | Prelude.otherwise ->
                             let _e
                                   = Thrift.ApplicationException (Text.pack (Prelude.show _ex))
                                       Thrift.ApplicationExceptionType_InternalError
                               in
                               (3, Thrift.buildStruct _proxy _e,
                                Prelude.Just (Exception.toException _e, Thrift.ServerError))
          Prelude.Right _result -> (2,
                                    Thrift.genStruct _proxy
                                      [Thrift.genFieldPrim _proxy "" (Thrift.getI64Type _proxy) 0 0
                                         (Thrift.genI64Prim _proxy)
                                         _result],
                                    Prelude.Nothing)
respWriter' _proxy _seqNum Foo{} _r
  = (Thrift.genMsgBegin _proxy "foo" _msgType _seqNum <> _msgBody <>
       Thrift.genMsgEnd _proxy,
     _msgException)
  where
    (_msgType, _msgBody, _msgException)
      = case _r of
          Prelude.Left _ex | Prelude.Just
                               _e@Thrift.ApplicationException{} <- Exception.fromException _ex
                             ->
                             (3, Thrift.buildStruct _proxy _e,
                              Prelude.Just (_ex, Thrift.ServerError))
                           | Prelude.Just _e@Types.Ex{} <- Exception.fromException _ex ->
                             (2,
                              Thrift.genStruct _proxy
                                [Thrift.genField _proxy "ex" (Thrift.getStructType _proxy) 1 0
                                   (Thrift.buildStruct _proxy _e)],
                              Prelude.Just (_ex, Thrift.ClientError))
                           | Prelude.otherwise ->
                             let _e
                                   = Thrift.ApplicationException (Text.pack (Prelude.show _ex))
                                       Thrift.ApplicationExceptionType_InternalError
                               in
                               (3, Thrift.buildStruct _proxy _e,
                                Prelude.Just (Exception.toException _e, Thrift.ServerError))
          Prelude.Right _result -> (2, Thrift.genStruct _proxy [],
                                    Prelude.Nothing)

onewayFunctions' :: [Text.Text]
onewayFunctions' = []