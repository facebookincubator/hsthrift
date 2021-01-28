-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.Dll
  ( SOExport
  , DllException(..)
  -- "static" shared object API
  , rotateDLL
  , loadDll
  , unloadObj
  -- "real" shared object API
  , loadNativeDll
  , unloadNativeObj
  , DLHandle
  , setHighMemDynamic
  ) where

import Control.Exception hiding (handle)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign
import GHCi.ObjLink
import Foreign.C.String
import System.Posix.Internals (CFilePath, withFilePath)

type SOExport a = IO (StablePtr a)

data DllException = DllException FilePath Text
  deriving (Show, Eq)

instance Exception DllException

-- Loading of "static" shared objects

-- | Given a previous dll and a new one:
-- * load in the new one
-- * pull out the exported data
-- * dump out the old dll if it existed
rotateDLL :: (Maybe FilePath, FilePath) -> Text -> IO a
rotateDLL (old, newDLL) sym = loadDll newDLL sym <* forM_ old unloadObj

loadDll :: FilePath -> Text -> IO a
loadDll dllPath symbol = do
  loadObj dllPath
  resolved <- resolveObjs
  unless resolved $ throwIO $ DllException dllPath "Unable to resolve objects"
  c_sym <- lookupSymbol $ Text.unpack symbol
  h <- case c_sym of
    Nothing ->
      throwIO $ DllException dllPath $ "Could not find symbol: " <> symbol
    Just p_sym ->
      bracket (mkCallable $ castPtrToFunPtr p_sym) freeStablePtr deRefStablePtr
  purged <- withFilePath dllPath c_purgeObj
  when (purged == 0) $ throwIO $ DllException dllPath "Unable to purge object"
  return h

-- Not exported by GHCI yet, so pull it in to dump the symbol table
foreign import ccall unsafe "purgeObj"
  c_purgeObj :: CFilePath -> IO Int

foreign import ccall "dynamic"
  mkCallable :: FunPtr (SOExport a) -> SOExport a

-- Loading of "real" shared objects

-- NB. if you're using this, you must add a dependency on
-- common/hs/dll:hs_dynamic_main.  See common/hs/dll/TARGETS.
loadNativeDll :: FilePath -> Text -> IO (a, DLHandle)
loadNativeDll dllPath symbol = do
  eHandle <- loadNativeObj dllPath
  handle <- case eHandle of
    Left msg -> throwIO $ DllException dllPath (Text.pack msg)
    Right h -> return h
  c_sym <- dlsym handle $ Text.unpack symbol
  h <- case c_sym of
    Nothing ->
      throwIO $ DllException dllPath $ "Could not find symbol: " <> symbol
    Just p_sym ->
      bracket (mkCallable $ castPtrToFunPtr p_sym)
        freeStablePtr deRefStablePtr
  return (h, handle)

-- I will have to export these in https://phabricator.haskell.org/D4263

type DLHandle = Ptr ()

loadNativeObj :: String -> IO (Either String DLHandle)
loadNativeObj str =
  withFilePath str $ \c_str ->
  alloca $ \perr -> do
    poke perr nullPtr
    bracket (c_loadNativeObj c_str perr) (\_ -> free =<< peek perr) $ \r -> do
      if r /= nullPtr
        then return $ Right r
        else do
          msg <- peekCString =<< peek perr
          return $ Left
            ("loadNativeObj " ++ show str ++ ": failed, reason: " ++ msg)

unloadNativeObj :: DLHandle -> IO ()
unloadNativeObj handle = do
  r <- c_unloadNativeObj handle
  when (r == 0) $ error "unloadNativeObj"

dlsym :: DLHandle -> String -> IO (Maybe (Ptr a))
dlsym handle str =
  withCAString str $ \c_str -> do
     addr <- c_dlsym handle c_str
     if addr == nullPtr
        then return Nothing
        else return (Just addr)


foreign import ccall unsafe "loadNativeObj"
  c_loadNativeObj :: CFilePath -> Ptr CString -> IO DLHandle

foreign import ccall unsafe "unloadNativeObj"
  c_unloadNativeObj :: DLHandle -> IO Int

foreign import ccall unsafe "dlsym"
  c_dlsym :: DLHandle -> CString -> IO (Ptr a)

foreign import ccall unsafe "setHighMemDynamic"
  setHighMemDynamic :: IO ()
