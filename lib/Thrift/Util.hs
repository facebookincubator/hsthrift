-- Copyright (c) Facebook, Inc. and its affiliates.

module Thrift.Util
  ( loadJSON
  , saveJSON
  , ThriftException(..)
  , prettyThrift
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import Thrift.Protocol
import Thrift.Protocol.JSON

newtype ThriftException = ThriftException String
    deriving Show

instance Exception ThriftException

-- | Load a Thrift value from a file using the PrettyJSON protocol
loadJSON :: ThriftSerializable a => FilePath -> IO a
loadJSON path = do
  result <- deserializeJSON <$> BS.readFile path
  case result of
    Left err -> throwIO $ ThriftException $ "[" ++ path ++ "] " ++ err
    Right struct -> return struct

-- | Save a Thrift value to a file using the PrettyJSON protocol
saveJSON :: ThriftSerializable a => FilePath -> a -> IO ()
saveJSON path value = BS.writeFile path $ serializePrettyJSON value

-- | Render a Thrift value as prettified JSON
prettyThrift :: ThriftSerializable a => a -> LB.ByteString
prettyThrift a =
  case eitherDecode (LB.fromStrict (serializeJSON a)) of
    Right obj -> encodePretty (obj :: Object)
    Left err -> LB.pack err
