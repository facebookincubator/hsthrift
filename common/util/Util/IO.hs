-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.IO
  ( atomicIO
  , die
  , readFileUTF8
  , writeFileUTF8
  , writeFileAtomicUTF8
  , readJSON
  , eitherReadJSON
  , writeJSON
  , writeJSON'
  , removeIfExists
  , readFileIfExists
  , writeFileIfChanged
  , writeFileAtomically
  , writeTextIfChanged
  , loudlyWriteTextIfChanged
  , writeTextIfChangedWith
  , writeUtf8StringIfChanged
  , loudlyWriteUtf8StringIfChanged
  , writeUtf8StringIfChangedWith
  , Verbosity(..)
  , writeFileUTF8Text
  , readFileUTF8Text
  , getDevserverUser
  , getDevserverHostInfo
  , getHostInfo
  , getHostname
  , getUserForProcess
  , getGroupForProcess
  , getUsername
  , getUserUnixname
  , HostInfo(..)
  , copyDirectoryContents
  , copyDirectoryContents_
  , listDirectoryRecursive
  , saveStdout
  , saveStderr
  , slowIO
  , isModifiedAfter
  , safeRemovePathForcibly
  ) where

import Control.Concurrent
import Control.Exception as Exception
import Control.Monad
import Control.Monad.Extra
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.IO.Handle
import System.Directory
import System.Exit hiding (die)
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Posix.User
import System.Process
import System.Environment
import Text.Printf
import Util.String (strip)
import qualified Data.ByteString.Lazy as B

-- | Performs some IO atomically. This is presumed to be only used
-- to avoid output interleaving where verbose information is printed
-- concurrently.
atomicIO :: IO a -> IO a
atomicIO = withMVar atomicIOLock . const

{-# NOINLINE atomicIOLock #-}
atomicIOLock :: MVar ()
atomicIOLock = unsafePerformIO $ newMVar ()

-- | Exit immediately with 'exitCode', showing a message on 'stderr'.
die :: Int -> String -> IO a
die exitCode msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure exitCode)

-- | Compare a file's content against some benchmark content to see
-- whether it's changed. You really want to pass in some kind of lazy
-- read function here so that (in-)equality can be determined as soon
-- as possible during the file read. Note that if the file doesn't
-- exist, then we return True, that is, we state that the file has
-- changed.
hasFileContentChanged ::
  (Eq a) => (FilePath -> IO a) -> FilePath -> a -> IO Bool
hasFileContentChanged read filePath benchmarkContent =
  catchIOError
    (liftM (/= benchmarkContent) $ read filePath)
    (\_ -> return True)

-- | Writes a file only if it differs from the content that would be
-- written.
writeFileIfChanged :: (Eq a) =>
  (FilePath -> IO a) -> (Handle -> a -> IO ()) -> FilePath -> a -> IO ()
writeFileIfChanged read hWrite targetPath content = do
  different <- hasFileContentChanged read targetPath content
  when different $ writeFileAtomically hWrite targetPath content

-- | Write a file "atomically" by writing it fully to a temp
-- directory, then copying it to its final location.
writeFileAtomically :: (Handle -> a -> IO ()) -> FilePath -> a -> IO ()
writeFileAtomically hWrite targetPath content =
    Exception.bracketOnError mkTempFile rmTempFile commit
  where
    (targetDir, targetFile) = splitFileName targetPath
    mkTempFile = openTempFile targetDir (targetFile <.> "tmp")
    rmTempFile (tmpPath, handle) = hClose handle >> removeFile tmpPath
    commit (tmpPath, handle) = do
      hWrite handle content
      hClose handle
      renameFile tmpPath targetPath

-- | Remove file if it exists. Returns True if the file existed.
removeIfExists :: FilePath -> IO Bool
removeIfExists filePath =
  (removeFile filePath >> return True) `catch` \e ->
    if isDoesNotExistError e
      then return False
      else throwIO e

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists filePath =
  (Just <$> readFile filePath) `catch` \e ->
    if isDoesNotExistError e
      then return Nothing
      else throwIO e

-- | Read a FromJSON datatype from a file containing a JSON string.
readJSON :: FromJSON a => FilePath -> IO a
readJSON f = do
  json <- B.readFile f
  case eitherDecode json of
    Left err -> error $ printf "Failed parsing JSON file %s: %s\n" f err
    Right v  -> return v

eitherReadJSON :: FromJSON a => FilePath -> IO (Either String a)
eitherReadJSON f = (eitherDecode <$> B.readFile f)
  `catch` (\(e :: Exception.IOException) -> return $ Left (show e))

-- |  Write a ToJSON datatype to a file as pretty-printed JSON.
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path = B.writeFile path . encodePretty

-- | Write a ToJSON datatype to a file as pretty-printed JSON.
-- Adds a newline at the end of the file
writeJSON' :: ToJSON a => Config -> FilePath -> a -> IO ()
writeJSON' config path obj = B.writeFile path (encodePretty' config obj <> "\n")

-- | Read a UTF-8 encoded file.  Like 'readFile' but forces the
-- encoding to UTF-8.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  hGetContents h

-- | Write a UTF-8 encoded file.  Like 'writeFile' but forces the
-- encoding to UTF-8.
writeFileUTF8 :: Handle -> String -> IO ()
writeFileUTF8 h str = do
    hSetEncoding h utf8
    hPutStr h str

-- | Write a UTF-8 encoded file atomically.  Writes to a temporary file
-- first, and then atomically moves the temporary file to the destination
-- if successful.
--
-- Adapted from Distribution.Simple.Utils.writeFileAtomic in Cabal.
writeFileAtomicUTF8 :: FilePath -> String -> IO ()
writeFileAtomicUTF8 targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  Exception.bracketOnError
    (do
      createDirectoryIfMissing True targetDir
      openBinaryTempFileWithDefaultPermissions targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        hSetEncoding handle utf8
        hPutStr handle content
        hClose handle
        renameFile tmpPath targetPath)

-- | Write a UTF-8 encoded file from text, only if it differs from the content
-- that would be written. If the output directory will be created if it does
-- not exist.
writeTextIfChanged :: FilePath -> Text -> IO ()
writeTextIfChanged path text = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFileIfChanged readFileUTF8Text writeFileUTF8Text path text

-- Like Util.IO.writeTextIfChanged, but announce upon success.
loudlyWriteTextIfChanged :: FilePath -> Text -> IO ()
loudlyWriteTextIfChanged = writeTextIfChangedWith Loud putStrLn

data Verbosity = Loud | Quiet

writeTextIfChangedWith
  :: Verbosity
  -> (String -> IO ())
  -> FilePath
  -> Text
  -> IO ()
writeTextIfChangedWith verbosity f path text = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFileIfChanged readFileUTF8Text writeFileUTF8Text_ path text
  where
  writeFileUTF8Text_ h txt = do
    writeFileUTF8Text h txt
    case verbosity of
      Loud -> atomicIO $ f ("Written " ++ path)
      Quiet -> return ()

-- | Write a UTF-8 encoded file from text.  Like 'writeFile' but forces the
-- encoding to UTF-8.
writeFileUTF8Text :: Handle -> Text -> IO ()
writeFileUTF8Text h text = do
  hSetEncoding h utf8
  TextIO.hPutStr h text

-- | Read a UTF-8 encoded file as text.
readFileUTF8Text :: FilePath -> IO Text
readFileUTF8Text path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  TextIO.hGetContents h

-- Like Util.IO.writeTextIfChanged, but works for String
writeUtf8StringIfChanged :: FilePath -> String -> IO ()
writeUtf8StringIfChanged path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFileIfChanged readFileUTF8 writeFileUTF8 path content

-- Like Util.IO.loudlyWriteTextIfChanged, but works for String.
loudlyWriteUtf8StringIfChanged :: FilePath -> String -> IO ()
loudlyWriteUtf8StringIfChanged = writeUtf8StringIfChangedWith Loud putStrLn

-- Like Util.IO.writeTextIfChangedWith, but works for String
writeUtf8StringIfChangedWith
  :: Verbosity
  -> (String -> IO ())
  -> FilePath
  -> String
  -> IO ()
writeUtf8StringIfChangedWith verbosity f path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFileIfChanged readFileUTF8 writeFileUTF8_ path content
  where
  writeFileUTF8_ h txt = do
    writeFileUTF8 h txt
    case verbosity of
      Loud -> atomicIO $ f ("Written " ++ path)
      Quiet -> return ()

getUserForProcess :: IO String
getUserForProcess = fmap userName $ getUserEntryForID =<< getRealUserID

getGroupForProcess :: IO String
getGroupForProcess = fmap groupName $ getGroupEntryForID =<< getRealGroupID

data HostInfo = HostInfo
  { hostname :: Text
  , username :: Text
  }

getHostInfo :: IO HostInfo
getHostInfo = do
  user <- Text.pack <$> getUsername
  host <- Text.pack <$> getHostname
  return $ HostInfo host user

getDevserverHostInfo :: IO HostInfo
getDevserverHostInfo = do
  user <- Text.pack <$> getDevserverUser
  host <- Text.pack <$> getHostname
  return $ HostInfo host user

-- | Equivalent of get_devserver_username in www (see
-- https://fburl.com/codex/05mqipn2).
getDevserverUser :: IO String
getDevserverUser = getUserForProcess

-- | Get username using 'id' command line tool
getUsername :: IO String
getUsername = init <$> readProcess "id" ["-un"] ""

-- | Get hostname using 'hostname' command line tool
getHostname :: IO String
getHostname = strip <$> readProcess "hostname" [] []

-- Try to get a meaninful user unix name. This will not realize that the user
-- was changed to a non-root account like mysql.
getUserUnixname :: IO String
getUserUnixname = do
  user <- getUserForProcess
  fromMaybe user <$> asum
    [ return $ helpful $ Just user
    , helpful <$> getTupperwareUser
    , helpful <$> getDevserverOwner
    ]
  where
    unhelpfulNames = ["root", "twsvcscm", "svcscm", "apache"]

    helpful (Just name) | name `elem` unhelpfulNames = Nothing
    helpful mname = mname

    getTupperwareUser = lookupEnv "TW_JOB_USER"

getDevserverOwner :: IO (Maybe String)
getDevserverOwner = do
  res <- Exception.try $ readFile "/etc/devserver.owners"
    :: IO (Either Exception.SomeException String)
  return $ case lines <$> res of
    Right (name:_) -> Just name
    _ -> Nothing

redirectHandle :: Handle -> FilePath -> IO () -> IO ()
redirectHandle handle file io =
  withFile file WriteMode $ \fh -> do
  bracket (hDuplicate handle) (`hDuplicateTo` handle) $ \_ -> do
    hDuplicateTo fh handle
    io

-- | Run an IO action saving the stderr to a file
saveStderr :: FilePath -> IO () -> IO ()
saveStderr = redirectHandle stderr

-- | Run an IO action saving the stdout to a file
saveStdout :: FilePath -> IO () -> IO ()
saveStdout = redirectHandle stdout

-- | Copy all the contents of one directory to another directory
-- and return all the resulting paths.
-- This will create the target directory if it does not exist.

-- | DISCLAIMER: This is not an atomic operation.
copyDirectoryContents :: FilePath -> FilePath -> IO [FilePath]
copyDirectoryContents dirfrom dirto = do
  paths <- listDirectoryRecursive dirfrom
  forM paths $ \path -> do
      let relfile = makeRelative dirfrom path
      let frompath = dirfrom </> relfile
      let topath = dirto </> relfile
      createDirectoryIfMissing True $ takeDirectory topath
      copyFile frompath topath
      return topath

copyDirectoryContents_ :: FilePath -> FilePath -> IO ()
copyDirectoryContents_ dirfrom dirto
  = void $ copyDirectoryContents dirfrom dirto

listDirectoryRecursive
    :: FilePath
    -> IO [FilePath]
listDirectoryRecursive dir = do
    fs <- listDirectory dir
    (concat <$>) $ forM fs $ \file -> do
        let f = dir </> file
        isFile <- doesFileExist f
        if isFile
        then return [f]
        else do
            isDir <- doesDirectoryExist f
            if isDir
            then listDirectoryRecursive f
            else return []


-- | Perform IO in batches with delays between them. Useful to avoid
-- filling up fixed size queues.
slowIO
  :: ([a] -> IO ())                     -- ^ operation to apply to each batch
  -> [a]                                -- ^ all arguments
  -> Int                                -- ^ maximum batch size
  -> Int                                -- ^ delay, in milliseconds
  -> IO ()
slowIO f args batchSize delay =
  sequence_
    $ intersperse (threadDelay (delay*1000))
    $ map f
    $ chunksOf batchSize args

isModifiedAfter :: FilePath -> FilePath -> IO Bool
isModifiedAfter fp1 fp2 = ifM (doesFileExist fp1 &&^ doesFileExist fp2)
  (do
    time <- getModificationTime fp1
    time' <- getModificationTime fp2
    return $ time > time')
  (return False)

-- | System.Posix.removeLink calls @unlink()@ with an unsafe FFI call,
-- which can cause long stalls in some cases. This affects things
-- defined in terms of it, including 'System.Directory.removeFile',
-- and 'System.Directory.removePathForcibly' to name a couple.
--
-- This function is a replacement for 'removePathForcibly' that forks
-- an external 'rm' instead.
safeRemovePathForcibly :: FilePath -> IO ()
safeRemovePathForcibly path = callProcess "rm" [ "-rf", path ]
