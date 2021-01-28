-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.Processor
  ( Processor(..)
  , Blame(..)
  , process
  , processCommand
  , msgParser
  , Some(..)
  ) where

#if __GLASGOW_HASKELL__ > 804
#define This Some
#endif

import Control.Exception
import Thrift.Binary.Parser
import Data.ByteString
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Int
import Data.Some
#if __GLASGOW_HASKELL == 804
import Data.Monoid ((<>))
#endif
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text

import Thrift.Protocol
import Thrift.Protocol.ApplicationException.Types

data Blame = ClientError | ServerError
  deriving (Eq,Ord,Enum,Bounded,Read,Show)

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
  -- | Oneway methods of this processor
  onewayFns :: Proxy s -> [Text]

-- | `process` should be called once for each received request
process :: (Processor s, Protocol p)
        => Proxy p  -- ^ The server's protocol to use
        -> SeqNum   -- ^ Sequence number
        -> (forall r . s r -> IO r)
           -- ^ Handler for user-code
        -> ByteString -- ^ Input bytes off the wire
        -> IO (ByteString, Maybe (SomeException, Blame))
            -- ^ Output bytes to put on the wire as well as the exception
            -- information for the response
process proxy seqNum handler input = do
  (response, exc) <- case parse (msgParser proxy) input of
    Left err -> do
      -- Parsing failed, so the protocol is broken
      let ex = ApplicationException (Text.pack err)
            ApplicationExceptionType_ProtocolError
      return
        ( genMsgBegin proxy "" 3 seqNum
          <> buildStruct proxy ex
          <> genMsgEnd proxy
        , Just (toException ex, ClientError) )
    Right (This cmd) -> processCommand proxy seqNum handler cmd
  return (toStrict (toLazyByteString response), exc)

processCommand
  :: (Processor s, Protocol p)
  => Proxy p
  -> SeqNum
  -> (forall r . s r -> IO r) -- ^ Handler for user-code
  -> s r                      -- ^ input command
  -> IO (Builder, Maybe (SomeException, Blame))
processCommand proxy seqNum handler cmd = do
  -- Run the handler and generate its return struct, forcing evaluation
  (builder, exc) <- respWriter proxy seqNum cmd <$> try (handler cmd)
  builder' <- evaluate builder
  return (builder', exc)

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
