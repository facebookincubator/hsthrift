{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Control.Trace.Eventlog (
  eventlogTracer,
  Trace (..),
) where

import Control.Monad.Catch (
  ExitCase (
    ExitCaseAbort,
    ExitCaseException
  ),
 )
import Control.Trace.Core (
  Tracer (traceMsg_),
 )
import Data.ByteString (ByteString)
import Data.String (fromString)
import Debug.Trace.Flags (userTracingEnabled)
import OpenTelemetry.Eventlog (
  beginSpan,
  endSpan,
  setTag,
 )

data Trace = Trace
  { name :: !ByteString
  , tags :: ![(ByteString, ByteString)]
  }

{- | A tracer that emits opentelemetry spans to the GHC eventlog.

   Compile with `-c fbcode.hs_eventlog=True`, run with `+RTS -l`
   and visualize the traces with `//common/hs/ghc-chrome-trace` or
   the opentelemetry-extra package.
-}
eventlogTracer :: Tracer Trace
eventlogTracer
  | userTracingEnabled = mempty {traceMsg_ = trace}
  | otherwise = mempty
  where
    trace Trace {..} = release <$> acquire
      where
        acquire = beginSpan name
        release sp res = do
          mapM_ (uncurry $ setTag sp) tags
          case res of
            ExitCaseException e -> do
              setTag sp "error" "1"
              setTag sp "errorMsg" (fromString $ show e)
            ExitCaseAbort ->
              setTag sp "abort " "1"
            _ -> pure ()
          endSpan sp
