{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ViewPatterns #-}
module Control.Trace.VLog (
  vlogTracer,
  TraceWithPriority (..),
  vlogTracerWithPriority,
  vlogShowTracer,
  vlogTextTracer,
) where

import Control.Monad (when)
import Control.Monad.Catch (
  ExitCase (
    ExitCaseAbort,
    ExitCaseException,
    ExitCaseSuccess
  ),
 )
import Control.Monad.IO.Class
import Control.Trace.Core
import Data.Some
import Data.Text (Text)
import GHC.Stack
import TextShow (
  TextShow,
  showt,
 )
import qualified Util.Log.String as String
import Util.Log.Text

vlogShowTracer :: TextShow a => (a -> Int) -> Tracer a
vlogShowTracer =
  vlogTracer
    (\(showt -> x) -> ("BEGIN " <> x, \e -> "END" <> renderExitCase e <> x))
    showt

renderExitCase :: Some ExitCase -> Text
renderExitCase (Some ExitCaseAbort {}) = "(aborted) "
renderExitCase (Some (ExitCaseException e)) = "(" <> showt e <> ") "
renderExitCase (Some ExitCaseSuccess {}) = " "

vlogTextTracer :: Int -> Tracer Text
vlogTextTracer p =
  vlogTracer
    (\x -> ("BEGIN " <> x, \e -> "END" <> renderExitCase e <> x))
    id
    (const p)

data TraceWithPriority
  = Skip
  | T !Int !Text
  | S !Int !String

vlogTracerWithPriority :: Tracer TraceWithPriority
vlogTracerWithPriority = Tracer {..}
  where
    logMsg_ :: (HasCallStack, MonadIO m) => TraceWithPriority -> m ()
    logMsg_ Skip = pure ()
    logMsg_ x = withFrozenCallStack $ case x of
      T p t -> vlog p t
      S p s -> String.vlog p s
      Skip -> error "unreachable"

    traceMsg_ msg =
      case msg of
        T p t -> do
            vlog p ("BEGIN " <> t)
            return ( \res -> case res of
                ExitCaseSuccess {} -> vlog p ("END " <> t)
                ExitCaseAbort {} -> vlog p ("ABORTED " <> t)
                ExitCaseException e -> vlog p ("FAILED " <> t <> ": " <> showt e)
              )
        S p t -> do
            String.vlog p ("BEGIN " <> t)
            return ( \res -> case res of
                ExitCaseSuccess {} -> String.vlog p ("END " <> t)
                ExitCaseAbort {} -> String.vlog p ("ABORTED " <> t)
                ExitCaseException e ->
                  String.vlog p ("FAILED " <> t <> ": " <> show e)
              )
        Skip -> return $ const $ return ()

vlogTracer ::
  forall a.
  -- | render BEGIN and END messages
  (a -> (Text, Some ExitCase -> Text)) ->
  -- | render LOG message
  (a -> Text) ->
  -- | Priority (use -1 to skip)
  (a -> Int) ->
  Tracer a
vlogTracer beginend log_ prio = Tracer {..}
  where
    logMsg_ :: a -> IO ()
    logMsg_ msg =
        let p = prio msg
         in when (p >= 0) $ vlog p $ log_ msg

    traceMsg_ :: a -> IO (ExitCase b -> IO ())
    traceMsg_ msg = do
      let p = prio msg
          (b, e) = beginend msg
      if p >= 0
        then vlog p b >> return (\res -> vlog p (e $ mkSome res))
        else return $ const $ return ()
