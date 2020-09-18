{-# LANGUAGE TemplateHaskell #-}
module Foreign.CPP.Destructible.TH
  ( module Destructible
  , deriveDestructibleSafe
  , deriveDestructibleUnsafe
  ) where

import Data.Char
import Foreign.Ptr
import Language.Haskell.TH

import Foreign.CPP.Destructible as Destructible

-- | This is a Template Haskell function which is used in conjunction with
-- @HS_DEFINE_DESTRUCTIBLE@ from "Destructible.h" to generate instances for
-- 'Destructible'. Use 'deriveDestructibleSafe' instead if the destructor
-- can block.
deriveDestructibleUnsafe :: String -> TypeQ -> Q [Dec]
deriveDestructibleUnsafe = deriveDestructible unsafe

-- | This is the safe version of 'deriveDestructibleUnsafe'. Use this instead
-- if the destructor can block.
deriveDestructibleSafe :: String -> TypeQ -> Q [Dec]
deriveDestructibleSafe = deriveDestructible safe

data CImport
  = CFunction String
  | CLabel String

cppBindingPrefix :: CImport -> String
cppBindingPrefix (CFunction cid) = "destructible_" ++ cid
cppBindingPrefix (CLabel cid) = '&': cppBindingPrefix (CFunction cid)

hsMethod :: CImport -> String
hsMethod (CFunction cid) = cid
hsMethod (CLabel cid) = hsMethod (CFunction cid) ++ "FunPtr"

hsMethodType :: CImport -> TypeQ -> TypeQ
hsMethodType cimp hsType = go cimp
  where
  go (CFunction _) = [t| Ptr $hsType -> IO () |]
  go (CLabel cid) = [t| FunPtr $(go $ CFunction cid) |]

deriveDestructible :: Safety -> String -> TypeQ -> Q [Dec]
deriveDestructible safety cppName hsType = do
  (funs, forImps) <- unzip <$> mapM gen
    [ CFunction "destruct"
    , CFunction "delete"
    , CLabel "delete"
    ]
  -- instance Destructible HsString where
  inst <- instanceD
    (cxt [])
    [t| Destructible $hsType |]
    (map pure funs)
  return $ inst: forImps
  where
  gen :: CImport -> Q (Dec, Dec)
  gen cimp = do
    hsName <- map (\i -> if isAlphaNum i then i else '_') . pprint <$> hsType
    let cppBinding = cppBindingPrefix cimp ++ cppName
        hsBinding = mkName $ hsMethod cimp ++ hsName ++ show safety
    -- destruct = destructHsStructUnsafe
    fun <- funD (mkName $ hsMethod cimp)
      [ clause [] (normalB $ varE hsBinding) []
      ]
    -- foreign import ccall unsafe "destructible_destructCppStruct"
    --   destructHsStructUnsafe :: Ptr Hs -> IO ()
    forImp <- forImpD cCall safety cppBinding hsBinding $
      hsMethodType cimp hsType
    return (fun, forImp)
