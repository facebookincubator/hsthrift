{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.Directory (
  Directory(..),
  listFilesRecursive,
) where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State
import qualified Data.HashSet as Set
import qualified System.Directory as IO
import System.FilePath

-- | An effect class for modelling directory I/O
class Directory f where
  doesDirectoryExist :: FilePath -> f Bool
  listDirectory :: FilePath -> f [FilePath]

instance Directory IO where
  doesDirectoryExist = IO.doesDirectoryExist
  listDirectory = IO.listDirectory

{- | A version of 'listFilesRecursive' from the 'extra' package offering more
   control and termination in the presence of cyclic symlinks.
-}
listFilesRecursive ::
  (Directory f, Monad f) =>
  -- | Whether to recurse in a directory
  (FilePath -> Bool) ->
  FilePath ->
  f [FilePath]
listFilesRecursive predDir = flip evalStateT mempty . go
 where
  go relDir = do
    visited <- get
    if Set.member relDir visited
      then return []
      else do
        modify $ Set.insert relDir
        ls <- lift $ map (relDir </>) <$> listDirectory relDir
        dirs <- lift $ filterM (\d -> (&& predDir d) <$> doesDirectoryExist d) ls
        (ls ++) <$> concatMapM go dirs

{-# SPECIALIZE
    listFilesRecursive :: (FilePath -> Bool) -> FilePath -> IO [FilePath] #-}
