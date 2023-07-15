{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}

module Foreign.CPP.HsStruct.HsMap (
  deriveHsHashMapUnsafe,
) where

import qualified Control.Monad
import Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as HashMap
import Language.Haskell.TH

import Foreign (Ptr)
import Foreign.C.Types (CSize)
import Foreign.CPP.Marshallable

-- HsHashMap -------------------------------------------------------------------

deriveHsHashMapUnsafe ::
  String -> TypeQ -> TypeQ -> Q [Dec]
deriveHsHashMapUnsafe cppType hsKeyType hsValType = do
  hashMapTypeStr <-
    map (\i -> if isAlphaNum i then i else '_') . pprint
      <$> hashMapType

  let cppConstructName = "map_construct" ++ cppName
      hsConstructName = mkName $ "c_map_construct" ++ hashMapTypeStr
  constructImport <-
    forImpD
      cCall
      unsafe
      cppConstructName
      hsConstructName
      [t|Ptr () -> CSize -> IO ()|]

  let cppAddName = "map_add" ++ cppName
      hsAddName = mkName $ "c_map_add" ++ hashMapTypeStr
  addImport <-
    forImpD
      cCall
      unsafe
      cppAddName
      hsAddName
      [t|Ptr () -> Ptr $hsKeyType -> Ptr $hsValType -> IO ()|]

  hsKT <- hsKeyType
  hsVT <- hsValType
  isKeyDestructible <- isInstance (mkName "Destructible") [hsKT]
  isValDestructible <- isInstance (mkName "Destructible") [hsVT]

  let keyWithFn = mkName $ if isKeyDestructible then "withCxxObject" else "with"
      valWithFn = mkName $ if isValDestructible then "withCxxObject" else "with"
      pN = mkName "p"
      hmN = mkName "hm"
      addChunk =
        [|
          Control.Monad.forM_ (HashMap.toList hm) $ \(key, val) ->
            $(varE keyWithFn) key $ \key_ptr ->
              $(varE valWithFn) val $ \val_ptr ->
                $(varE hsAddName) (castPtr p) key_ptr val_ptr
          |]

      -- newValue (HsHashMap _h) = error "HsHashMap cannot be made on the heap"
      newValueFn =
        funD
          (mkName "newValue")
          [ clause
              [conP (mkName "HsHashMap") [varP $ mkName "_h"]]
              (normalB [|error "HsHashMap cannot be made on the heap"|])
              []
          ]

      -- constructValue p (HsHashMap hm) = do
      --    $(varE hsConstructName) (castPtr p) (fromIntegral (length hm))
      --    Control.Monad.forM_ (toList hm) $ (\key, \val) ->
      --      $(varE withKeyFn) key $ \key_ptr ->
      --         $(varE withValFn) val $ \val_ptr ->
      --            $(varE hsAddName) (castPtr p) key_ptr val_ptr
      constructValueFn =
        funD
          (mkName "constructValue")
          [ clause
              [varP pN, conP (mkName "HsHashMap") [varP hmN]]
              ( normalB $
                  doE
                    [ noBindS
                        [|
                          $(varE hsConstructName)
                            (castPtr p)
                            (fromIntegral (length hm))
                          |]
                    , noBindS addChunk
                    ]
              )
              []
          ]

  -- instance Constructible (HsHashMap <TYPE>) where
  constructibleHashMapInst <-
    instanceD
      (cxt [])
      [t|Constructible $hashMapType|]
      [ newValueFn
      , constructValueFn
      ]

  return
    [ constructibleHashMapInst
    , constructImport
    , addImport
    ]
  where
    hashMapType = conT (mkName "HsHashMap") `appT` hsKeyType `appT` hsValType
    cppName = "HsMap" ++ cppType
