-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ForeignFunctionInterface #-}

module Util.GFlags
  ( FlagException(..)
  , getFlagValue
  , setFlagDefault
  , setFlagValue
  , setFlagValueIfDefault
  , withFlagSaver
  ) where

import Control.Exception
import Data.Text (Text)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Foreign.CPP.HsStruct
import qualified Util.Text as Text

--  export

data FlagException = FlagException Text deriving (Eq, Show)
instance Exception FlagException

getFlagValue :: Text -> IO (Either FlagException Text)
getFlagValue k = do
  Text.useTextAsCString k $ \ ck -> do
  withHsText $ \ out -> do
  c_get_flag_value ck out >>= \ case
    0 -> Left <$> pure (FlagException k)
    _ -> Right <$> hsText <$> peek out

setFlagDefault :: Text -> Text -> IO (Either FlagException ())
setFlagDefault = resultOfWithCStringKV c_set_flag_default

setFlagValue :: Text -> Text -> IO (Either FlagException ())
setFlagValue = resultOfWithCStringKV c_set_flag_value

setFlagValueIfDefault :: Text -> Text -> IO (Either FlagException ())
setFlagValueIfDefault = resultOfWithCStringKV c_set_flag_value_if_default

withFlagSaver :: IO a -> IO a
withFlagSaver = bracketConst c_flag_saver_create c_flag_saver_delete

--  utility

data FlagSaver -- C++ gflags::FlagSaver

withHsText :: (Ptr HsText -> IO a) -> IO a
withHsText = bracket c_hs_string_create c_hs_string_delete

resultOfWithCStringKV
  :: (CString -> CString -> IO CInt) -> Text -> Text
  -> IO (Either FlagException ())
resultOfWithCStringKV cb k v = result k <$> withCStringKV k v cb

withCStringKV :: Text -> Text -> (CString -> CString -> IO a) -> IO a
withCStringKV k v cb = do
  Text.useTextAsCString k $ \ ck -> do
  Text.useTextAsCString v $ \ cv -> do
  cb ck cv

result :: Text -> CInt -> Either FlagException ()
result k 0 = Left (FlagException k)
result _ _ = Right ()

bracketConst :: IO a -> (a -> IO b) -> IO c -> IO c
bracketConst before after = bracket before after . const

--  ffi

foreign import ccall unsafe
  "facebook_gflags_hs_set_flag_default"
  c_set_flag_default :: CString -> CString -> IO CInt

foreign import ccall unsafe
  "facebook_gflags_hs_set_flag_value"
  c_set_flag_value :: CString -> CString -> IO CInt

foreign import ccall unsafe
  "facebook_gflags_hs_set_flag_value_if_default"
  c_set_flag_value_if_default :: CString -> CString -> IO CInt

foreign import ccall unsafe
  "facebook_gflags_hs_get_flag_value"
  c_get_flag_value :: CString -> Ptr HsText -> IO CInt

foreign import ccall unsafe
  "facebook_gflags_hs_flag_saver_create"
  c_flag_saver_create :: IO (Ptr FlagSaver)

foreign import ccall unsafe
  "facebook_gflags_hs_flag_saver_delete"
  c_flag_saver_delete :: Ptr FlagSaver -> IO ()

foreign import ccall unsafe
  "facebook_gflags_hs_hs_string_create"
  c_hs_string_create :: IO (Ptr HsText)

foreign import ccall unsafe
  "facebook_gflags_hs_hs_string_delete"
  c_hs_string_delete :: Ptr HsText -> IO ()
