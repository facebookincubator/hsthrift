-- Copyright (c) Facebook, Inc. and its affiliates.

module Util.FilePath
  ( DirEnv(..)
  , getDirEnv
  , absolutise
  , absolutiseWith
  , mnameToPath
  , pathToMName
  ) where

import Data.Char (isAsciiUpper)
import Data.Text (Text)
import Data.Text as Text (pack, unpack)

import System.Directory
import System.FilePath

data DirEnv = DirEnv
  { homeDir :: FilePath
  , currentDir :: FilePath
  }

getDirEnv :: IO DirEnv
getDirEnv = DirEnv <$> getHomeDirectory <*> getCurrentDirectory

-- | Makes a path absolute, handling home directories
absolutiseWith :: DirEnv -> FilePath -> FilePath
absolutiseWith DirEnv{..} "~" = homeDir
absolutiseWith DirEnv{..} ('~':'/':p) = normalise $ homeDir </> p
absolutiseWith DirEnv{..} p | isRelative p = normalise $ currentDir </> p
absolutiseWith _ p = p

-- | Get the absolute FilePath based on the directory env
--
-- >>> absolutise "~/si_sigma/A.hs"
-- "{HOME_DIR}/si_sigma/A.hs"
-- >>> absolutise "sigma/repo/Foo/Bar/Baz.hs"
-- "{CURRENT_DIR}/sigma/repo/Foo/Bar/Baz.hs"
absolutise :: FilePath -> IO FilePath
absolutise p = absolutiseWith <$> getDirEnv <*> pure p

-- | Given a FilePath to a module, returns the expected module name
--
-- >>> pathToMName "sigma/repo/Foo/Bar/Baz.hs"
-- "Foo.Bar.Baz"
pathToMName :: FilePath -> Text
pathToMName = Text.pack . map convSeparator . getModulePath . dropExtension
  where
    convSeparator c = if isPathSeparator c then '.' else c
    getModulePath = joinPath . filterModulePaths . splitPath
    filterModulePaths = reverse . takeWhile isModulePath . reverse
    isModulePath (c:_) = isAsciiUpper c
    isModulePath _ = False

-- | Given a module name, returns the FilePath
--
-- >>> mnameToPath "Foo.Bar.Baz"
-- "Foo/Bar/Baz.hs"
mnameToPath :: Text -> FilePath
mnameToPath =
  (<> ".hs") . map (\c -> if c == '.' then pathSeparator else c) . Text.unpack
