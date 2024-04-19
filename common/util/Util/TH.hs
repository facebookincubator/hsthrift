{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Util.TH
  ( addTypeSuffix
  -- * Specialize
  , spec
  -- * Expose
  , exposeName
  , exposeVarE
  , exposeConE
  , exposeConT
  ) where

import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

addTypeSuffix :: Type -> [Dec] -> [Dec]
addTypeSuffix t decs =
  [ PragmaD $ RuleP (addSuffix n) tys tms lhs rhs phases
  | PragmaD (RuleP n tys tms lhs rhs phases) <- decs
  ]
  where
    addSuffix n = n <> " " <> showsPrec 99 t ""

-- | For each type from the provided tyle list, replace the placeholder with
-- given name in the rule with the type to create a group of rules. e.g.
--
-- > $(TH.spec "tv" [ [t| Int |], [t| Double |] ] [d|
-- >   {-# RULES "+" forall a b. a + b = a OPERATOR.+ b :: tv #-} |])
--
-- will produce
--
-- > {-# RULES "+ @Int" forall a b. a + b = a OPERATOR.+ b :: Int #-}
-- > {-# RULES "+ @Double" forall a b. a + b = a OPERATOR.+ b :: Double #-}
spec :: String -> [TypeQ] -> DecsQ -> DecsQ
spec name typeQs decsQ = do
  types <- sequence typeQs
  decs <- decsQ
  return
    [ rule
    | dec <- decs
    , ty <- types
    , rule <- addTypeSuffix ty [subst name ty dec]
    ]

subst :: String -> Type -> Dec -> Dec
subst from to = go
  where
    go :: Data a => a -> a
    go = gmapT go `extT` goT

    goT :: Type -> Type
    goT t = case t of
      WildCardT | from == "_" -> to
      VarT v | from == nameBase v -> to
      _ -> gmapT go t

-- | Exposes an unexported name. We use the provided hint 'Name' to determine
-- the package unit id to use. The name is specified as either a qualified or
-- an unqualified 'String'. When it is unqualified, it inherits the module name
-- from the provided hint. However, compiler created illegal variable names
-- are not allowed, e.g. @$dmrnf@ or @C:NFData@.
exposeName :: NameSpace -> Name -> String -> Q Name
exposeName ns (Name _ hint) name =
  case hint of
    NameG _ pkg m -> do
      let
        mkFlavor = NameG ns pkg
      return $ case mkName name of
        Name occ (NameQ mn) -> Name occ $ mkFlavor mn
        Name occ _ -> Name occ $ mkFlavor m
    _ -> fail $ "exposeName: invalid hint: " <> show hint

-- | Reference to a function name that is otherwise not visible via importing.
--
-- >>> $(exposeVarE ''Typeable "mkTrType")
-- *
exposeVarE :: Name -> String -> Q Exp
exposeVarE hint name = VarE <$> exposeName VarName hint name

-- | Reference to a data constructor that is otherwise not visible via
-- importing.
--
-- >>> $(ConE <$> exposeName DataName ''Typeable "TrType")
-- *
exposeConE :: Name -> String -> Q Exp
exposeConE hint name = ConE <$> exposeName DataName hint name

-- | Reference to a type constructor that is otherwise not visible via
-- importing.
--
-- >>> type T = $(ConT <$> exposeName TcClsName 'rnf "GNFData")
-- type T :: * -> (* -> *) -> Constraint
-- type T = Control.DeepSeq.GNFData :: * -> (* -> *) -> Constraint
exposeConT :: Name -> String -> Q Type
exposeConT hint name = ConT <$> exposeName TcClsName hint name
