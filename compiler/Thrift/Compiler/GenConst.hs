--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- License); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

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
