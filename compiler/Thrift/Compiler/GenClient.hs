-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}

module Thrift.Compiler.GenClient
  ( genClientDecls
  , genClientImports
  ) where

import Language.Haskell.Exts
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Text as Text

import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types hiding (Decl(..))

genClientImports :: Text.Text -> HS Service -> Set.Set Import
genClientImports this Service{..} =
  case resolvedName . fst . supResolvedName <$> serviceSuper of
    Nothing -> Set.empty
    Just (UName n) -> mkImports this n
    Just (QName m n) -> mkImports m n
  where
    mkImports m n = Set.fromList
      [ QImport (Text.intercalate "." [m, n, "Client"]) n
      ]

genClientDecls :: HS Service -> [Decl ()]
genClientDecls Service{..} =
  [ DataDecl () (DataType ()) Nothing (DHead () (textToName serviceName)) []
    mzero
  ] ++
  case serviceSuper of
    Nothing -> []
    Just Super{..} ->
      [ TypeInsDecl ()
        (qualType "Thrift" "Super" `appT` simpleType serviceName)
        (case resolvedName . fst $ supResolvedName of
           UName n -> qualType n n
           QName _ n -> qualType n n)
      ]
