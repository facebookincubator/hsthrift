-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

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

    traceMsg_ :: (HasCallStack, MonadTrace m) => TraceWithPriority -> m b -> m b
    traceMsg_ Skip act = act
    traceMsg_ msg act = withFrozenCallStack $ do
      case msg of
        T p t ->
          bracketM
            (vlog p ("BEGIN " <> t))
            ( \() res -> case res of
                ExitCaseSuccess {} -> vlog p ("END " <> t)
                ExitCaseAbort {} -> vlog p ("ABORTED " <> t)
                ExitCaseException e -> vlog p ("FAILED " <> t <> ": " <> showt e)
            )
            (\() -> act)
        S p t ->
          bracketM
            (String.vlog p ("BEGIN " <> t))
            ( \() res -> case res of
                ExitCaseSuccess {} -> String.vlog p ("END " <> t)
                ExitCaseAbort {} -> String.vlog p ("ABORTED " <> t)
                ExitCaseException e ->
                  String.vlog p ("FAILED " <> t <> ": " <> show e)
            )
            (\() -> act)
        Skip -> error "unreachable"

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
    logMsg_ :: (HasCallStack, MonadIO m) => a -> m ()
    logMsg_ msg =
      withFrozenCallStack $
        let p = prio msg
         in when (p >= 0) $ vlog p $ log_ msg

    traceMsg_ :: (HasCallStack, MonadTrace m) => a -> m b -> m b
    traceMsg_ msg act = withFrozenCallStack $ do
      let p = prio msg
          (b, e) = beginend msg
      if p >= 0
        then bracketM (vlog p b) (\() res -> vlog p (e $ mkSome res)) (\() -> act)
        else act
