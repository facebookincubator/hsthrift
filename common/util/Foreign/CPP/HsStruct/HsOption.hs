{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}
module Foreign.CPP.HsStruct.HsOption
  ( HsOption(..)
  , deriveHsOptionUnsafe
  ) where

import Data.Char (isAlphaNum)
import Foreign
import Language.Haskell.TH

import Foreign.CPP.Addressable
import Foreign.CPP.Marshallable

-- HsOption --------------------------------------------------------------------

newtype HsOption a = HsOption
  { hsOption :: Maybe a
  }

-- | This is a Template Haskell function which is used in conjunction with
-- @HS_OPTION_H@ and @HS_OPTION_CPP@ from "HsOption.h" to generate instances for
-- `HsOption T` for your type `T`
deriveHsOptionUnsafe
  :: String -> Int -> Int -> TypeQ -> Q [Dec]
deriveHsOptionUnsafe cppType sizeVal alignmentVal hsType = do
  optionTypeStr <-
    map (\i -> if isAlphaNum i then i else '_') . pprint <$> optionType

  -- instance Addressable (HsOption <TYPE>)
  addressableInst <- instanceD
    (cxt [])
    [t| Addressable $optionType |]
    []

  let
    -- sizeOf _ = #{size DummyHsOption<NAME>}
    sizeOfFn = funD (mkName "sizeOf")
      [ clause [wildP] (normalB $ litE $ integerL $ fromIntegral sizeVal) [] ]

    -- alignment _ = #{alignment DummyHsOption<NAME>}
    alignmentFn = funD (mkName "alignment")
      [ clause [wildP] (normalB $
          litE $ integerL $ fromIntegral alignmentVal) []
      ]

    -- poke = error "HsOption <TYPE> not pokeable"
    pokeFn = funD (mkName "poke")
      [ clause [] (normalB $
            varE (mkName "error") `appE`
            stringE (optionTypeStr ++ " not pokeable")
          )
          []
      ]

    pN = mkName "p"
    ptrN = mkName "ptr"
    peekN = mkName "peek"
    -- peek p = do
    --   ptr <- peekByteOff p 0  -- assume beginning of struct
    --   HsOption <$> maybePeek peek ptr
    peekFn = funD peekN
      [clause [varP pN] (normalB $ doE
        [ bindS
          (varP ptrN )
          (varE (mkName "peekByteOff") `appE` varE pN `appE` litE (integerL 0))
        , noBindS (infixApp
          (conE $ mkName "HsOption")
          (varE $ mkName "fmap")
          (varE (mkName "maybePeek") `appE` varE peekN `appE` varE ptrN)
          )
        ]) []]

  --  instance Storable (HsOption <TYPE>) where
  storableInst <- instanceD
    (cxt [])
    [t| Storable $optionType |]
    [sizeOfFn, alignmentFn, pokeFn, peekFn]

  let
    cppCtorName = "option_ctorHsOption" ++ cppType
    -- cppCtor = "void " ++ cppCtorName ++ "(void*, " ++ cppType ++ "*)"
    hsCtorName = mkName $ "c_ctor" ++ optionTypeStr

  -- foreign import ccall unsafe "<cppCtorName>"
  --   c_ctor<TYPE> :: Ptr (HsOption <TYPE>) -> Ptr <TYPE> -> IO ()
  ctorImport <- forImpD cCall unsafe cppCtorName hsCtorName
      [t| Ptr $optionType -> Ptr $hsType -> IO () |]

  let
    cppNewName = "option_newHsOption" ++ cppType
    -- cppNew = "void* " ++ cppNewName ++ "(" ++ cppType ++ "*)"
    hsNewName = mkName $ "c_new" ++ optionTypeStr

-- foreign import ccall unsafe "<cppNewName>"
--   c_new<TYPE> :: Ptr <TYPE> -> IO (Ptr (HsOption <TYPE>))
  newImport <- forImpD cCall unsafe cppNewName hsNewName
      [t| Ptr $hsType -> IO (Ptr $optionType) |]

  hsT <- hsType
  isDestructible <- isInstance (mkName "Destructible") [hsT]

  let
    optN = mkName "HsOption"
    nothingN = mkName "Nothing"
    justN = mkName "Just"
    vN = mkName "v"

    withFn = if isDestructible then "withCxxObject" else "with"

    -- newValue (HsOption Nothing) = newDefault
    -- newValue (HsOption (Just v)) =
    --   withCxxObject v $ c_newOption
    newValueFn = funD (mkName "newValue")
      [ clause [conP optN [conP nothingN []]] (normalB $
          varE (mkName "newDefault")) [],
        clause [conP optN [conP justN [varP vN]]] (normalB $
          infixApp
          (varE (mkName withFn) `appE` varE vN)
          (varE (mkName "$"))
          (varE hsNewName)
          ) []
      ]

    -- constructValue p (HsOption Nothing) = constructDefault p
    -- constructValue p (HsOption (Just v)) =
    --   withCxxObject v $ c_ctorOption p
    constructValueFn = funD (mkName "constructValue")
      [ clause [varP pN, conP optN [conP nothingN []]] (normalB $
          varE (mkName "constructDefault") `appE` varE pN) [],
        clause [varP pN, conP optN [conP justN [varP vN]]] (normalB $
          infixApp
          (varE (mkName withFn) `appE` varE vN)
          (varE (mkName "$"))
          (varE hsCtorName `appE` varE pN)
          ) []
      ]

  --  instance Constructible (HsOption <TYPE>) where
  constructibleInst <- instanceD
    (cxt [])
    [t| Constructible $optionType |]
    [newValueFn, constructValueFn]

  return
    [ addressableInst
    , storableInst
    , ctorImport
    , newImport
    , constructibleInst
    ]
  where
    optionType = [t| HsOption $hsType |]
