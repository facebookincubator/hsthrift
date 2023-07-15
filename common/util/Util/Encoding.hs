{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Encoding where

import GHC.IO.Encoding
  ( setForeignEncoding
  , setLocaleEncoding
  , utf8
  )

-- | Set the foreign/locale encoding used by GHC
-- If affects:
-- 1. Any handle in text mode that has not an explicit encoding set
--    including stdin, stdout and stderr
-- 2. Marshalling `String` to `CString` via functions in `Foreign.C.String`
setDefaultEncodingToUTF8 :: IO ()
setDefaultEncodingToUTF8 = do
  setForeignEncoding utf8
  setLocaleEncoding utf8

foreign export ccall "hs_setDefaultEncodingToUTF8"
  setDefaultEncodingToUTF8 :: IO ()
