-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}
module Foreign.CPP.HsStruct.HsArray
  ( deriveHsArrayUnsafe
  ) where


import Control.Monad (forM_)
import Data.Char (isAlphaNum)
import Data.Vector (forM_)
import Language.Haskell.TH

import Foreign
import Foreign.C.Types (CSize)
import Foreign.CPP.Marshallable


deriveHsArrayUnsafe
  :: String -> TypeQ -> Q [Dec]
deriveHsArrayUnsafe cppType hsType = do
  arrayTypeStr <-
    map (\i -> if isAlphaNum i then i else '_') . pprint <$> arrayType

  let
    cppNewName = "vector_new" ++ cppName
    hsNewName = mkName $ "c_vector_new" ++ arrayTypeStr
  newImport <- forImpD cCall unsafe cppNewName hsNewName
      [t| CSize -> IO (Ptr ()) |]

  let
    cppConstructName = "vector_construct" ++ cppName
    hsConstructName = mkName $ "c_vector_construct" ++ arrayTypeStr
  constructImport <- forImpD cCall unsafe cppConstructName hsConstructName
      [t| Ptr () -> CSize -> IO () |]

  let
    cppAddName = "vector_add" ++ cppName
    hsAddName = mkName $ "c_vector_add" ++ arrayTypeStr
  addImport <- forImpD cCall unsafe cppAddName hsAddName
      [t| Ptr () -> Ptr $hsType -> IO () |]

  hsT <- hsType
  isDestructible <- isInstance (mkName "Destructible") [hsT]

  let
    withFn = mkName $ if isDestructible then "withCxxObject" else "with"

    pN = mkName "p"
    vsN = mkName "vs"
    fromIntegralChunk = [| fromIntegral (length vs) |]
    addChunk forMFn =
      [| $forMFn vs $ \v ->
           $(varE withFn) v $ \v_ptr ->
             $(varE hsAddName) (castPtr p) v_ptr
      |]

    newValueFn forMFn cType = funD (mkName "newValue")
      [clause [conP (mkName cType) [varP vsN]] (normalB $ doE
        [ bindS (varP pN)
            [| castPtr <$> $(varE hsNewName) $(fromIntegralChunk)|]
        , noBindS (addChunk forMFn)
        , noBindS [| return p |]
        ]) []]

    constructValueFn forMFn cType = funD (mkName "constructValue")
      [clause [varP pN, conP (mkName cType) [varP vsN]] (normalB $ doE
        [ noBindS [| $(varE hsConstructName) (castPtr p) $(fromIntegralChunk)|]
        , noBindS (addChunk forMFn)
        ]) []]


  --  instance Constructible (HsList <TYPE>) where
  constructibleListInst <- instanceD
    (cxt [])
    [t| Constructible $listType |]
    [ newValueFn [| Control.Monad.forM_ |] "HsList"
    , constructValueFn [| Control.Monad.forM_ |] "HsList"
    ]

  --  instance Constructible (HsArray <TYPE>) where
  constructibleArrayInst <- instanceD
    (cxt [])
    [t| Constructible $arrayType |]
    [ newValueFn [| Data.Vector.forM_ |] "HsArray"
    , constructValueFn [| Data.Vector.forM_ |] "HsArray"
    ]

  return
    [ constructibleListInst
    , constructibleArrayInst
    , newImport
    , constructImport
    , addImport
    ]
  where
    listType = conT (mkName "HsList") `appT` hsType
    arrayType = conT (mkName "HsArray") `appT` hsType
    cppName = "HsArray" ++ cppType
