{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Thrift.Monad
  ( ThriftM
  , ThriftEnv(..)
  , RpcOptions(..), Priority(..)
  , defaultRpcOptions, setRpcPriority, getRpcPriority
  , runThrift, runThriftWith, withOptions, catchThrift
  , bracketThrift, bracketThrift_, tryThrift
  , Counter, newCounter
  , type (<:), Super
  ) where

import Control.Exception
import Control.Monad.Trans.Reader
import Data.Int
import Data.IORef
import Data.Proxy
import Foreign.C
import GHC.TypeLits
import Thrift.Protocol.RpcOptions.Types (RpcOptions(..))
import Util.Reader

#include <common/hs/thrift/lib/cpp/HsChannel.h>

-- Thrift Monad ----------------------------------------------------------------

type ThriftM p c s = ReaderT (ThriftEnv p c s) IO

data ThriftEnv p c s = ThriftEnv
  { thriftProxy   :: Proxy p
  , thriftChannel :: c s
  , thriftRpcOpts :: RpcOptions
  , thriftCounter :: Counter
  }

data Priority
  = HighImportant
  | High
  | Important
  | NormalPriority
  | BestEffort
    deriving (Enum, Ord, Eq, Show)

getCppPriority :: Priority -> Int32
getCppPriority HighImportant = fromIntegral ((#const HIGH_IMPORTANT) :: CInt)
getCppPriority High = fromIntegral ((#const HIGH) :: CInt)
getCppPriority Important = fromIntegral ((#const IMPORTANT) :: CInt)
getCppPriority NormalPriority = fromIntegral ((#const NORMAL) :: CInt)
getCppPriority BestEffort = fromIntegral ((#const BEST_EFFORT) :: CInt)

defaultRpcOptions :: RpcOptions
defaultRpcOptions = RpcOptions
  { rpc_timeout      = 0
  , rpc_priority     = Nothing
  , rpc_chunkTimeout = 0
  , rpc_queueTimeout = 0
  , rpc_headers      = Nothing
  }

getRpcPriority :: RpcOptions -> Maybe Priority
getRpcPriority RpcOptions{..} = case rpc_priority of
  Nothing -> Nothing
  Just (#const HIGH_IMPORTANT) -> Just HighImportant
  Just (#const HIGH) -> Just High
  Just (#const IMPORTANT) -> Just Important
  Just (#const NORMAL) -> Just NormalPriority
  Just (#const BEST_EFFORT) -> Just BestEffort
  _ -> Just NormalPriority

setRpcPriority :: RpcOptions -> Priority -> RpcOptions
setRpcPriority opts@RpcOptions{..} prio =
  case rpc_priority of
    Just _ -> opts
    Nothing -> opts{rpc_priority = Just $ getCppPriority prio}

runThrift :: ThriftM p c s a -> c s -> IO a
runThrift action channel = runThriftWith action channel defaultRpcOptions

runThriftWith :: ThriftM p c s a -> c s -> RpcOptions -> IO a
runThriftWith action channel opts =
  newCounter >>= runReaderT action . ThriftEnv Proxy channel opts

withOptions :: RpcOptions -> ThriftM p c s a -> ThriftM p c s a
withOptions opts = withReaderT (\env -> env { thriftRpcOpts = opts })

catchThrift
  :: Exception e => ThriftM p c s a -> (e -> ThriftM p c s a) -> ThriftM p c s a
catchThrift = catchR

bracketThrift
  :: ThriftM p c s a        -- ^ before
  -> (a -> ThriftM p c s b) -- ^ after
  -> (a -> ThriftM p c s d) -- ^ computation
  -> ThriftM p c s d
bracketThrift = bracketR

bracketThrift_
  :: ThriftM p c s a -- ^ before
  -> ThriftM p c s b -- ^ after
  -> ThriftM p c s d -- ^ computation
  -> ThriftM p c s d
bracketThrift_ = bracketR_

tryThrift :: Exception e => ThriftM p c s a -> ThriftM p c s (Either e a)
tryThrift m = (Right <$> m) `catchThrift` (pure . Left)

-- Seqeuence Counters ----------------------------------------------------------

type Counter = IO Int32

newCounter :: IO Counter
newCounter = do
  ref <- newIORef 0
  return $ do
    count <- readIORef ref
    writeIORef ref (count + 1)
    return count

-- Subtyping of Services -------------------------------------------------------


-- | The subtyping constraint
class a <: b

-- | Supertype Relation
type family Super (s :: *) :: *

type family IsSuper a b (n :: Nat) :: Bool where
  IsSuper a b 0 = 'False
  IsSuper a a n = 'True
  IsSuper a b n = IsSuper (Super a) b (n - 1)

type MaxChainSize = 100

instance (IsSuper a b MaxChainSize ~ 'True) => a <: b
