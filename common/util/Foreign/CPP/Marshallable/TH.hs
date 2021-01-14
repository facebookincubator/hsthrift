-- Copyright (c) Facebook, Inc. and its affiliates.

{- |
This module provides facilities to auto generate instances for
@DefaultConstructible@ and @Destructible@ typeclasses. It depends
on certain functions being exposed via C++, which can also be generated
by using C++ macros defined in 'Marshallable.h'.
-}
{-# LANGUAGE TemplateHaskell #-}
module Foreign.CPP.Marshallable.TH
  ( module Marshallable
    -- Generate both DefaultConstructible and Destructible instances
  , deriveMarshallableUnsafe
  , deriveMarshallableSafe
    -- Generate Destructible instance
  , deriveDestructibleSafe
  , deriveDestructibleUnsafe
    -- Generate DefaultConstructible instance
  , deriveDefConstructibleUnsafe
  , deriveDefConstructibleSafe
  ) where

import Control.Monad (liftM2)
import Data.Char
import Foreign.Ptr
import Language.Haskell.TH

import Foreign.CPP.Marshallable as Marshallable

-- | This is a Template Haskell function which is used in conjunction with
-- @HS_DEFINE_DESTRUCTIBLE@ from "Destructible.h" to generate instances for
-- 'Destructible'. Use 'deriveDestructibleSafe' instead if the destructor
-- can block.
deriveDestructibleUnsafe :: String -> TypeQ -> Q [Dec]
deriveDestructibleUnsafe = derive destructSpec unsafe

-- | This is the safe version of 'deriveDestructibleUnsafe'. Use this instead
-- if the destructor can block.
deriveDestructibleSafe :: String -> TypeQ -> Q [Dec]
deriveDestructibleSafe = derive destructSpec safe

-- | This is a Template Haskell function which is used in conjunction with
-- @HS_DEFINE_DEFAULT_CONSTRUCTIBLE@ from "Construcible.h" to generate
-- instances for 'DefaultConstrucible'. Use 'deriveDefConstructibleSafe'
-- instead if the constructor can block.
deriveDefConstructibleUnsafe :: String -> TypeQ -> Q [Dec]
deriveDefConstructibleUnsafe = derive constructSpec unsafe

-- | This is the safe version of 'deriveDefConstructibleUnsafe'. Use this
-- instead if the constructor can block.
deriveDefConstructibleSafe :: String -> TypeQ -> Q [Dec]
deriveDefConstructibleSafe = derive constructSpec safe

-- | Function to generate both @DefaultConstructible@ and @Destructible@
-- instances.
deriveMarshallableUnsafe :: String -> TypeQ -> Q [Dec]
deriveMarshallableUnsafe cppName hsType = liftM2 (++)
  (deriveDefConstructibleUnsafe cppName hsType)
  (deriveDestructibleUnsafe cppName hsType)

-- | Safe version of @deriveMarshallableUnsafe@.
deriveMarshallableSafe :: String -> TypeQ -> Q [Dec]
deriveMarshallableSafe cppName hsType = liftM2 (++)
  (deriveDefConstructibleSafe cppName hsType)
  (deriveDestructibleSafe cppName hsType)

-- Specializations for different typeclasses ----------------------------------

data TypeClassSpec = TypeClassSpec
  { tySpec_classNm :: TypeQ
  , tySpec_functions :: [HsMethod]
  }

data HsMethod = HsMethod
  { hs_name :: String
  , hs_cppPrefix :: String
  , hs_typeFn :: TypeQ -> TypeQ
  }

destructSpec :: TypeClassSpec
destructSpec = TypeClassSpec
  { tySpec_classNm = [t| Destructible |]
  , tySpec_functions =
      [ HsMethod "destruct" "destructible_destruct"
          (\hsType -> [t| Ptr $hsType -> IO () |])
      , HsMethod "delete" "destructible_delete"
          (\hsType -> [t| Ptr $hsType -> IO () |])
      , HsMethod "deleteFunPtr" "&destructible_delete"
          (\hsType -> [t| FunPtr (Ptr $hsType -> IO ()) |])
      ]
  }

constructSpec :: TypeClassSpec
constructSpec = TypeClassSpec
  { tySpec_classNm = [t| DefaultConstructible |]
  , tySpec_functions =
      [ HsMethod "constructDefault" "constructible_constructDefault"
          (\hsType -> [t| Ptr $hsType -> IO () |])
      , HsMethod "newDefault" "constructible_newDefault"
          (\hsType -> [t| IO (Ptr $hsType) |])
      ]
  }

-- Common code ----------------------------------------------------------------

derive :: TypeClassSpec -> Safety -> String -> TypeQ -> Q [Dec]
derive TypeClassSpec{..} safety cppName hsType = do
  (funs, forImps) <- unzip <$> mapM gen tySpec_functions
  -- instance Destructible HsString where
  instD <- instanceD
    (cxt [])
    [t| $tySpec_classNm $hsType |]
    (map pure funs)
  return $ instD: forImps
  where
  gen :: HsMethod -> Q (Dec, Dec)
  gen HsMethod{..} = do
    hsTypeStr <- map (\i -> if isAlphaNum i then i else '_') . pprint <$> hsType
    let cppBinding = hs_cppPrefix ++ cppName
        hsBinding = mkName $ hs_name ++ hsTypeStr ++ show safety
    -- destruct = destructHsStructUnsafe
    fun <- funD (mkName hs_name)
      [ clause [] (normalB $ varE hsBinding) []
      ]
    -- foreign import ccall unsafe "destructible_destructCppStruct"
    --   destructHsStructUnsafe :: Ptr Hs -> IO ()
    forImp <- forImpD cCall safety cppBinding hsBinding $
      hs_typeFn hsType
    return (fun, forImp)
