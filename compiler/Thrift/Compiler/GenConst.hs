{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Compiler.GenConst
  ( genConstImports
  , genConstDecl
  ) where

import Language.Haskell.Exts.Syntax
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Syntax as HS

import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types

genConstImports :: HS Const -> Set.Set Import
genConstImports Const{..} =
  Set.union baseImports (typeToImport constResolvedType)
  where
    baseImports = Set.fromList
      [ QImport "Data.Default" "Default"
      ]

genConstDecl :: HS Const -> [HS.Decl ()]
genConstDecl Const{..} =
  -- Type Signature
  [ TypeSig () [ textToName constResolvedName ] (genType constResolvedType)
  -- Const Decl
  , FunBind ()
    [ Match () (textToName constResolvedName) []
      (UnGuardedRhs () $ genConst constResolvedType constResolvedVal)
      Nothing
    ]
  ]
