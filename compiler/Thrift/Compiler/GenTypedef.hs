{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Thrift.Compiler.GenTypedef
  ( genTypedefDecl
  , genTypedefImports
  ) where

import Control.Monad
import Data.Maybe
import Data.Set (union)
import Data.Text (Text)
import Language.Haskell.Exts.Syntax hiding (Type)
import qualified Data.Set as Set

import Thrift.Compiler.GenStruct
import Thrift.Compiler.GenUtils
import Thrift.Compiler.Plugins.Haskell
import Thrift.Compiler.Types hiding (Decl(..))

genTypedefImports :: HS Typedef -> Set.Set Import
genTypedefImports Typedef{..} =
  typeToImport tdResolvedType `union`
  case tdTag of
    IsTypedef -> Set.empty
    IsNewtype -> Set.fromList
      [ QImport "Prelude" "Prelude"
      , QImport "Control.DeepSeq" "DeepSeq"
      , QImport "Data.Aeson" "Aeson"
      , QImport "Data.Hashable" "Hashable"
      ]

genTypedefDecl :: HS Typedef -> Bool -> [Decl ()]
genTypedefDecl Typedef{..} deriveShow = case tdTag of
  IsTypedef ->
    [ TypeDecl () (DHead () $ textToName tdResolvedName)
      (genType tdResolvedType)
    ]
  IsNewtype ->
    [ DataDecl () (NewType ()) Nothing (DHead () name)
      -- Constructor Declaration
      [ QualConDecl () Nothing Nothing
        (RecDecl () name
         [ FieldDecl () [ textToName ("un" <> tdResolvedName) ]
           (genType tdResolvedType)
         ])
      ]
      -- Deriving
      (pure $ deriving_ $ map (IRule () Nothing Nothing . IHCon ()) $ catMaybes
        [ qualSym "Prelude" "Eq"     <$ guard True
        , qualSym "Prelude" "Show"   <$ guard deriveShow
        , qualSym "DeepSeq" "NFData" <$ guard True
        , qualSym "Prelude" "Ord"    <$ guard deriveOrd
        ])
      -- Instances
    , genHashable tdResolvedType tdResolvedName
    , genToJSON tdResolvedType tdResolvedName
    ] ++
    [ genOrd tdResolvedType tdResolvedName | not deriveOrd ]
  where
    name = textToName tdResolvedName
    deriveOrd = isNothing $ mkOrd tdResolvedType

-- Hashable --------------------------------------------------------------------

genHashable :: HSType t -> Text -> Decl ()
genHashable ty alias =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp () (IHCon () (qualSym "Hashable" "Hashable")) $
   simpleType alias)
  (Just
   [ InsDecl () $ FunBind ()
     [ Match () (textToName "hashWithSalt")
       [ pvar "__salt"
       , PApp () (unqualSym alias) [ pvar "__val" ]
       ]
       (UnGuardedRhs () $
        qvar "Hashable" "hashWithSalt" `app`
        var "__salt" `app`
        transformValue mkHashable Default ty (var "__val"))
       Nothing
     ]
   ])

-- Ord -------------------------------------------------------------------------

genOrd :: HSType t -> Text -> Decl ()
genOrd ty alias =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp () (IHCon () (qualSym "Prelude" "Ord")) $
   simpleType alias)
  (Just
   [ InsDecl () $ FunBind ()
     [ Match () (textToName "compare")
       [ PApp () (unqualSym alias) [ pvar "__x" ]
       , PApp () (unqualSym alias) [ pvar "__y" ]
       ]
       (UnGuardedRhs () $
        qvar "Prelude" "compare" `app`
        transformValue mkOrd Default ty (var "__x") `app`
        transformValue mkOrd Default ty (var "__y"))
       Nothing
     ]
   ])

-- Ord -------------------------------------------------------------------------

genToJSON :: HSType t -> Text -> Decl ()
genToJSON ty alias =
  InstDecl () Nothing
  (IRule () Nothing Nothing $
   IHApp () (IHCon () (qualSym "Aeson" "ToJSON")) $
   simpleType alias)
  (Just
   [ InsDecl () $ FunBind ()
     [ Match () (textToName "toJSON")
       [ PApp () (unqualSym alias) [ pvar "__val" ] ]
       (UnGuardedRhs () $ app (qvar "Aeson" "toJSON") $
        case fixToJSONValue ty of
          Nothing -> var "__val"
          Just f  -> f `app` var "__val")
       Nothing
     ]
   ])
