-- Copyright (c) Facebook, Inc. and its affiliates.

--
-- | Support for building a Haxl datasource to connect to a Thrift
-- service.
--
module Haxl.DataSource.Thrift
  ( dispatchSingle
  , dispatchList, dispatchList'
  , dispatchMap, dispatchMap'
  , dispatchVoid, dispatchVoid'
  , dispatchOneway, dispatchOneway'
  , dispatchCommon
  , sendCobSingle, sendCobOneway, recvCobSingle
  , sendCobBatch, recvCobList, recvCobMap, recvCobVoid
  , putFetchErrors
  , ThriftTransientError(..), ThriftLogicError(..)
  , wrapError
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import TextShow
import qualified Data.HashMap.Strict as HashMap

import Haxl.Core.Exception
import Haxl.Core

import Thrift.Channel
import Thrift.Monad
import Thrift.Protocol
import Thrift.Protocol.ApplicationException.Types

-- Exceptions ------------------------------------------------------------------

newtype ThriftTransientError a =
  ThriftTransientError { unThriftTransientError :: a }
  deriving (Show, Eq)
instance (Show a, Typeable a) => Exception (ThriftTransientError a) where
  toException = transientErrorToException
  fromException = transientErrorFromException

thriftTransientError :: (Show a, Typeable a) => a -> SomeException
thriftTransientError = toException . ThriftTransientError

newtype ThriftLogicError a =
  ThriftLogicError { unThriftLogicError :: a }
  deriving (Show, Eq)
instance (Show a, Typeable a) => Exception (ThriftLogicError a) where
  toException = logicErrorToException
  fromException = logicErrorFromException

thriftLogicError :: (Show a, Typeable a) => a -> SomeException
thriftLogicError = toException . ThriftLogicError

-- Dispatch Functions ----------------------------------------------------------

-- | Type of a generated thrift send_ function
type Send p s a =
  forall c . (Protocol p, ClientChannel c)
  => Proxy p
  -> c s
  -> Counter
  -> SendCallback
  -> RecvCallback
  -> RpcOptions
  -> a
  -> IO ()

-- | Type of a generated thrift send_ function
type SendOneway p s a =
  forall c . (Protocol p, ClientChannel c)
  => Proxy p
  -> c s
  -> Counter
  -> SendCallback
  -> RpcOptions
  -> a
  -> IO ()

-- | Type of a generated thrift recv_ function
type Recv p a = Proxy p -> Response -> Either SomeException a

-- | Dispatch a list of single (unbatched) requests
dispatchSingle
  :: (Protocol p, ClientChannel c)
  => [(ResultVar a, req)]
  -> Send p s req
  -> Recv p a
  -> ThriftM p c s ()
dispatchSingle requests send recv = do
  forM_ requests $ \(rvar, req) ->
    dispatchCommon send
      (sendCobSingle rvar)
      (recvCobSingle rvar . recv)
      req

-- | Dispatch a batched request where the response compes back as a list of
-- results
dispatchList
  :: (Protocol p, ClientChannel c)
  => Text -- ^ request name
  -> req
  -> [ResultVar a]
  -> Send p s req
  -> Recv p [a]
  -> ThriftM p c s ()
dispatchList name request rvars send recv = unless (null rvars) $
  dispatchCommon send
    (sendCobBatch rvars)
    (recvCobList name rvars . recv)
    request

dispatchList'
  :: (Protocol p, ClientChannel c)
  => Text -- ^ request name
  -> HashMap (Maybe RpcOptions) [(a, ResultVar b)]
  -> Send p s [a]
  -> Recv p [b]
  -> ThriftM p c s ()
dispatchList' name reqs send recv =
  dispatchWithOptions send (\_ rvars -> recvCobList name rvars . recv) reqs

-- | Dispatch a batched request where the response comes back in a hashmap from
-- key to result
dispatchMap
  :: (Eq k, Hashable k, TextShow k, Protocol p, ClientChannel c)
  => Text         -- ^ request name
  -> (req -> [k]) -- ^ extract keys from request
  -> req
  -> [ResultVar a]
  -> Send p s req
  -> Recv p (HashMap k a)
  -> ThriftM p c s ()
dispatchMap name getKeys request rvars send recv = unless (null rvars) $
  dispatchCommon send
    (sendCobBatch rvars)
    (recvCobMap name (getKeys request) rvars . recv)
    request

dispatchMap'
  :: (Eq a, Hashable a, TextShow a, Protocol p, ClientChannel c)
  => Text         -- ^ request name
  -> HashMap (Maybe RpcOptions) [(a, ResultVar b)]
  -> Send p s [a]
  -> Recv p (HashMap a b)
  -> ThriftM p c s ()
dispatchMap' name reqs send recv =
  dispatchWithOptions send (\req rvars -> recvCobMap name req rvars . recv) reqs

-- | Dispatch a batched request where the response is void
dispatchVoid
  :: (Protocol p, ClientChannel c)
  => [(a, ResultVar ())]
  -> Send p s [a]
  -> Recv p ()
  -> ThriftM p c s ()
dispatchVoid reqs send recv = unless (null reqs) $
  dispatchCommon send
    (sendCobBatch rvars)
    (recvCobVoid rvars . recv)
    req
  where
    (req, rvars) = unzip reqs

-- | Dispatch a batched request where the response is void
dispatchVoid'
  :: (Protocol p, ClientChannel c)
  => HashMap (Maybe RpcOptions) [(a, ResultVar ())]
  -> Send p s [a]
  -> Recv p ()
  -> ThriftM p c s ()
dispatchVoid' reqs send recv =
  dispatchWithOptions send (\_ rvars -> recvCobVoid rvars . recv) reqs

dispatchOneway
  :: (Protocol p, ClientChannel c)
  => [(a, ResultVar ())]
  -> SendOneway p s [a]
  -> ThriftM p c s ()
dispatchOneway reqs send = unless (null reqs) $ do
  ThriftEnv{..} <- ask
  lift $ send thriftProxy thriftChannel thriftCounter
    (sendCobBatchOneway rvars)
    thriftRpcOpts
    req
  where
    (req, rvars) = unzip reqs

dispatchOneway'
  :: (Protocol p, ClientChannel c)
  => HashMap (Maybe RpcOptions) [(a, ResultVar ())]
  -> SendOneway p s [a]
  -> ThriftM p c s ()
dispatchOneway' allReqs send =
  forM_ (HashMap.toList allReqs) $ \(opts, reqs) -> do
    let
      (req, rvars) = unzip reqs
    unless (null reqs) $ do
      ThriftEnv{..} <- ask
      lift $ send thriftProxy thriftChannel thriftCounter
        (sendCobBatchOneway rvars)
        (fromMaybe thriftRpcOpts opts)
        req

dispatchWithOptions
  :: forall p c s a b. (Protocol p, ClientChannel c)
  => Send p s [a]
  -> ([a] -> [ResultVar b] -> Proxy p -> RecvCallback)
  -> HashMap (Maybe RpcOptions) [(a, ResultVar b)]
  -> ThriftM p c s ()
dispatchWithOptions send recvCob allReqs =
  forM_ (HashMap.toList allReqs) $ \(opts, reqs) -> do
    let
      (req, rvars) = unzip reqs
      send' :: Send p s [a]
      send' = case opts of
        Nothing -> send
        Just o -> \p s c sc rc _ -> send p s c sc rc o
    unless (null req) $
      dispatchCommon send'
        (sendCobBatch rvars)
        (recvCob req rvars)
        req

dispatchCommon
  :: (Protocol p, ClientChannel c)
  => Send p s req
  -> SendCallback
  -> (Proxy p -> RecvCallback)
  -> req
  -> ThriftM p c s ()
dispatchCommon send sendCob recvCob request = do
  ThriftEnv{..} <- ask
  lift $ send thriftProxy thriftChannel thriftCounter
    sendCob
    (recvCob thriftProxy)
    thriftRpcOpts
    request

-- Generic Callbacks for non-Batched Calls -------------------------------------

sendCobSingle :: ResultVar a -> SendCallback
sendCobSingle _ Nothing = return ()
sendCobSingle result (Just err) =
  putResultFromChildThread result $ Left $ thriftTransientError err

sendCobOneway :: ResultVar () -> SendCallback
sendCobOneway result Nothing = putResultFromChildThread result $ Right ()
sendCobOneway result (Just err) =
  putResultFromChildThread result $ Left $ thriftTransientError err

recvCobSingle
  :: ResultVar a -> (Response -> Either SomeException a) -> RecvCallback
recvCobSingle result _ (Left err) =
  putResultFromChildThread result $ Left $ thriftTransientError err
recvCobSingle result deserialize (Right response) = do
  val <- case deserialize response of
    Left err -> Left <$> wrapError err
    res@Right{} -> return res
  putResultFromChildThread result val

-- Generic Callbacks for Batched Calls -----------------------------------------

sendCobBatch :: [ResultVar a] -> Maybe ChannelException -> IO ()
sendCobBatch _ Nothing = return ()
sendCobBatch rvars (Just e) = putChannelExceptions rvars e

sendCobBatchOneway :: [ResultVar ()] -> SendCallback
sendCobBatchOneway rvars Nothing = mapM_ (`putResult` Right ()) rvars
sendCobBatchOneway rvars (Just e) = putChannelExceptions rvars e

recvCobList
  :: Text
  -> [ResultVar a]
  -> (Response -> Either SomeException [a])
  -> Either ChannelException Response
  -> IO ()
recvCobList _ rvars _ (Left e) = putChannelExceptions rvars e
recvCobList name rvars recv (Right r) = case recv r of
  Left e -> putWrappedErrors rvars e
  Right rs
    | length rs == length rvars ->
        zipWithLastM_ putResult putResultFromChildThread rvars (map Right rs)
    | otherwise ->
        putChannelExceptions rvars (if null rs then err_empty else err_size)
  where
  err_empty = ChannelException $
    name <> ": response was empty"
  err_size = ChannelException $
    name <> ": response length does not match request length"

recvCobMap
  :: (Eq k, Hashable k, TextShow k)
  => Text
  -> [k]
  -> [ResultVar a]
  -> (Response -> Either SomeException (HashMap k a))
  -> Either ChannelException Response
  -> IO ()
recvCobMap _ _ rvars _ (Left e) = putChannelExceptions rvars e
recvCobMap name keys rvars recv (Right r) =
  case recv r of
    Left e -> putWrappedErrors rvars e
    Right m -> zipWithLastM_ (f putResult m) (f putResultFromChildThread m) keys rvars
  where
    f put m key rvar =
      put rvar $
        case HashMap.lookup key m of
          Nothing  -> Left $ thriftTransientError $
                      ChannelException $
                      name <> ": no response for key " <> showt key
          Just val -> Right val

recvCobVoid
  :: [ResultVar ()]
  -> (Response -> Either SomeException ())
  -> Either ChannelException Response
  -> IO ()
recvCobVoid rvars _ (Left e) = putChannelExceptions rvars e
recvCobVoid rvars recv (Right r) =
  zipWithLastM_ putResult putResultFromChildThread rvars $ repeat $ recv r

putChannelExceptions :: [ResultVar a] -> ChannelException -> IO ()
putChannelExceptions rvars e =
  mapLastM_ (f putResult) (f putResultFromChildThread) rvars
 where
  f put a = put a (Left (thriftTransientError e))

putWrappedErrors :: [ResultVar a] -> SomeException -> IO ()
putWrappedErrors rvars e =
  mapLastM_ (f putResult) (f putResultFromChildThread) rvars
 where
  f put a = put a . Left =<< wrapError e

putFetchErrors :: [BlockedFetch req] -> SomeException -> IO ()
putFetchErrors reqs e =
  mapLastM_ (f putResult) (f putResultFromChildThread) reqs
 where
  f :: (forall a . ResultVar a -> Either SomeException a -> IO ())
    -> BlockedFetch u -> IO ()
  f put (BlockedFetch _ rvar) = put rvar . Left =<< wrapError e

wrapError :: SomeException -> IO SomeException
wrapError err@(SomeException e)
  | Just ae@SomeAsyncException{} <- fromException err = throwIO ae
  | Just ce@ChannelException{} <- fromException err =
      return $ thriftTransientError ce
  | Just ce@ApplicationException{ applicationException_type = t }
      <- fromException err
  , isTransient t = return $ thriftTransientError ce
  -- This contains ApplicationException and all other exception types defined in
  -- the `throws` clause of the Thrift function
  | otherwise = return $ thriftLogicError e
  where
  isTransient ApplicationExceptionType_Timeout = True
  isTransient ApplicationExceptionType_Loadshedding = True
  isTransient _ = False


-- zipWithM_ with a different function to call for the last element
zipWithLastM_ :: (a -> b -> IO ()) -> (a -> b -> IO ()) -> [a] -> [b] -> IO ()
zipWithLastM_ f flast (a0:as0) (b0:bs0) = go a0 b0 as0 bs0
 where
  go a b (a':as) (b':bs) = f a b >> go a' b' as bs
  go a b _ _ = flast a b
zipWithLastM_ _ _ _ _ = return ()

-- mapM_  with a different function to call for the last element
mapLastM_ :: (a -> IO ()) -> (a -> IO ()) -> [a] -> IO ()
mapLastM_ _ _ [] = return ()
mapLastM_ f flast (a0:as0) = go a0 as0
 where
  go a [] = flast a
  go a (a':as) = f a >> go a' as
