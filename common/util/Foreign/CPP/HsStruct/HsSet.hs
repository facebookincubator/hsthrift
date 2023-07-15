{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}

module Foreign.CPP.HsStruct.HsSet (
  deriveHsHashSetUnsafe,
) where

import qualified Control.Monad
import Data.Char (isAlphaNum)
import Language.Haskell.TH

import Foreign (Ptr)
import Foreign.C.Types (CSize)
import Foreign.CPP.Marshallable

-- HsHashSet -------------------------------------------------------------------

deriveHsHashSetUnsafe ::
  String -> TypeQ -> Q [Dec]
deriveHsHashSetUnsafe cppType hsType = do
  hashSetTypeStr <-
    map (\i -> if isAlphaNum i then i else '_') . pprint
      <$> hashSetType

  let cppConstructName = "set_construct" ++ cppName
      hsConstructName = mkName $ "c_set_construct" ++ hashSetTypeStr
  constructImport <-
    forImpD
      cCall
      unsafe
      cppConstructName
      hsConstructName
      [t|Ptr () -> CSize -> IO ()|]

  let cppAddName = "set_add" ++ cppName
      hsAddName = mkName $ "c_set_add" ++ hashSetTypeStr
  addImport <-
    forImpD
      cCall
      unsafe
      cppAddName
      hsAddName
      [t|Ptr () -> Ptr $hsType -> IO ()|]

  hsT <- hsType
  isDestructible <- isInstance (mkName "Destructible") [hsT]

  let withFn = mkName $ if isDestructible then "withCxxObject" else "with"
      pN = mkName "p"
      keysN = mkName "keys"

      addChunk =
        [|
          Control.Monad.forM_ keys $ \key ->
            $(varE withFn) key $ \key_ptr ->
              $(varE hsAddName) (castPtr p) key_ptr
          |]

      -- newValue (HsHashSet _h) = error "HsHashSet cannot be made on the heap"
      newValueFn =
        funD
          (mkName "newValue")
          [ clause
              [conP (mkName "HsHashSet") [varP $ mkName "_h"]]
              (normalB [|error "HsHashSet cannot be made on the heap"|])
              []
          ]

      -- constructValue (HsHashSet h) = do
      --    $(varE hsConstructName) (castPtr p) (fromIntegral (length keys)
      --    Control.Monad.forM_ keys $ \key ->
      --      $(varE withFn) key $ \key_ptr ->
      --        $(varE hsAddName) (castPtr p) key_ptr
      constructValueFn =
        funD
          (mkName "constructValue")
          [ clause
              [varP pN, conP (mkName "HsHashSet") [varP keysN]]
              ( normalB $
                  doE
                    [ noBindS
                        [|
                          $(varE hsConstructName)
                            (castPtr p)
                            (fromIntegral (length keys))
                          |]
                    , noBindS addChunk
                    ]
              )
              []
          ]

  -- instance Constructible (HsHashSet <TYPE>) where
  constructibleHashSetInst <-
    instanceD
      (cxt [])
      [t|Constructible $hashSetType|]
      [ newValueFn
      , constructValueFn
      ]

  return
    [ constructibleHashSetInst
    , constructImport
    , addImport
    ]
  where
    hashSetType = conT (mkName "HsHashSet") `appT` hsType
    cppName = "HsSet" ++ cppType
