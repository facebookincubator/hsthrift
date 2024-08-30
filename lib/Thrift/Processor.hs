{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Thrift.Processor
  ( Processor(..)
  , Blame(..)
  , Header
  , process
  , processCommand
  , msgParser
  , Some(..)
  , MethodInfo(..)
  ) where

import Control.Exception
import Thrift.Binary.Parser
import Data.ByteString
import Data.ByteString.Builder
#if __GLASGOW_HASKELL__ < 902
import Data.ByteString.Lazy (toStrict)
#endif
import Data.Int
import Data.Some
import Data.Map (Map)
#if __GLASGOW_HASKELL == 804
import Data.Monoid ((<>))
#endif
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text

import Thrift.Channel (Header)
import Thrift.Protocol
import Thrift.Protocol.ApplicationException.Types
import Thrift.Monad (Priority(..))

data Blame = ClientError | ServerError
  deriving (Eq,Ord,Enum,Bounded,Read,Show)

data MethodInfo = MethodInfo
  { methodPriority :: Priority
  , methodIsOneway :: Bool
  }

-- | Class of types that can handle parsing + running thrift requests
class Processor s where
  -- | Returns the name for a particular function
  reqName :: s a -> Text
  -- | Parses the structure based on "text" name into a command
  reqParser :: Protocol p => Proxy p -> Text -> Parser (Some s)
  -- | Generate the serialized response for the returned structure as well as
  -- exception information
  respWriter
    :: Protocol p
    => Proxy p
    -> Int32
    -> s a
    -> Either SomeException a
    -> (Builder, Maybe (SomeException, Blame))
  -- | Static information about a method
  methodsInfo :: Proxy s -> Map Text MethodInfo
  methodsInfo = const mempty

-- | `process` should be called once for each received request
process :: (Processor s, Protocol p)
        => Proxy p  -- ^ The server's protocol to use
        -> SeqNum   -- ^ Sequence number
        -> (forall r . s r -> IO r)
           -- ^ Handler for user-code
        -> (forall r . s r -> Either SomeException r -> Header)
        -> ByteString -- ^ Input bytes off the wire
        -> IO (ByteString, Maybe (SomeException, Blame), Header)
            -- ^ Output bytes to put on the wire as well as the exception
            -- information for the response
process proxy seqNum handler postProcess input = do
  (response, exc, headers) <- case parse (msgParser proxy) input of
    Left err -> do
      -- Parsing failed, so the protocol is broken
      let ex = ApplicationException (Text.pack err)
            ApplicationExceptionType_ProtocolError
      return
        ( genMsgBegin proxy "" 3 seqNum
          <> buildStruct proxy ex
          <> genMsgEnd proxy
        , Just (toException ex, ClientError)
        , [] )
    Right (Some cmd) -> processCommand proxy seqNum handler postProcess cmd
  return (toStrict (toLazyByteString response), exc, headers)

processCommand
  :: (Processor s, Protocol p)
  => Proxy p
  -> SeqNum
  -> (forall r . s r -> IO r) -- ^ Handler for user-code
  -> (forall r . s r -> Either SomeException r -> Header)
  -> s r                      -- ^ input command
  -> IO (Builder, Maybe (SomeException, Blame), Header)
processCommand proxy seqNum handler postProcess cmd = do
  -- Run the handler and generate its return struct, forcing evaluation
  res <- try (handler cmd)
  let (builder, exc) = respWriter proxy seqNum cmd res
      headers = postProcess cmd res
  builder' <- evaluate builder
  return (builder', exc, headers)

msgParser
  :: (Processor s, Protocol p)
  => Proxy p -> Parser (Some s)
msgParser proxy = do
  MsgBegin funName msgTy _ <- parseMsgBegin proxy
  command <- case msgTy of
    1 -> reqParser proxy funName
    2 -> fail $ Text.unpack $ funName <> " expected call but got reply"
    3 -> fail $ Text.unpack $ funName <> " expected call but got exception"
    4 -> reqParser proxy funName
    _ -> fail $ Text.unpack $ funName <> ": invalid message type"
  parseMsgEnd proxy
  return command
