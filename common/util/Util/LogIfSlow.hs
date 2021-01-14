-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Util.LogIfSlow (logIfSlow, checkUnsafe) where

import Data.Text (Text)
import Control.Monad
import qualified Data.Text as Text
import Text.Printf
import Util.Timing (timeItNoGC)
import Util.Log.Text as Log

-- | Logs a message (via @Util.Log.log info@) if the given operation takes
-- longer than a specified threshold in seconds.
--
logIfSlow
  :: Double    -- ^ threshold in seconds
  -> Text      -- ^ description, for the log message
  -> IO a      -- ^ the operation to time
  -> IO a
logIfSlow threshold fun io = do
  (t, _, a) <- timeItNoGC io
  when (t > threshold) $
    Log.logInfo $ fun <> " took " <> Text.pack (printf "%.2fs" t)
  return a

-- | Logs a message if an unsafe foreign call takes longer than 1ms,
-- which indicates that it should probably not be marked "unsafe".
--
-- > f = checkUnsafe "c_foo" $ c_foo x y z
-- > foreign import ccall unsafe "foo" c_foo :: ...
--
-- will log a message (via 'Util.Log.info') if the call to @c_foo@ takes
-- longer than 1ms.
--
checkUnsafe :: Text -> IO a -> IO a

#ifdef ENABLE_CHECKUNSAFE

checkUnsafe fun = logIfSlow badUnsafeCallThreshold msg
  where msg = "unsafe foreign call " <> fun

badUnsafeCallThreshold :: Double
badUnsafeCallThreshold = 0.001

#else

checkUnsafe _ io = io
{-# INLINE checkUnsafe #-}

#endif
