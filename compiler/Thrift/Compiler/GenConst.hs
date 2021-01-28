-- Copyright (c) Facebook, Inc. and its affiliates.

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
genConstImports Const{..} = typeToImport constResolvedType

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
