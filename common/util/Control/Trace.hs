{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | A tracing library inspired by dcoutts Contravariant logging talk[1]
--   and the co-log package.
--
--   [1] -  https://www.youtube.com/watch?v=qzOQOmmkKEM
--
--  Example of usage:
--
--  > tracer :: Tracer Text
--  > tracer = vlogTextTracer 1
--  >
--  > main = traceMsg tracer "main" $ do
--  >   putStrLn "Hello world"
--
module Control.Trace
  ( Tracer
  , logMsg
  , traceMsg
  , (>$<)
  , vlogTracer
  , vlogTracerWithPriority
  , TraceWithPriority(..)
  , vlogShowTracer
  , vlogTextTracer
  ) where

import Control.Trace.Core
import Control.Trace.VLog
