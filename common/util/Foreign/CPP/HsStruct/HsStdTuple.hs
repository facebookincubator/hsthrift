{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TemplateHaskell #-}
module Foreign.CPP.HsStruct.HsStdTuple
  ( deriveHsStdTupleUnsafe
  , HsStdTuple(..)
  ) where

import Data.List (foldl')
import Control.Monad (forM, unless)
import Foreign
import Language.Haskell.TH

import Foreign.CPP.Addressable
import Foreign.CPP.Marshallable


newtype HsStdTuple a = HsStdTuple { unHsStdTuple :: a }

deriveHsStdTupleUnsafe
  :: String -> Int -> TypeQ -> Q [Dec]
deriveHsStdTupleUnsafe cppType sizeVal hsType = do
  hsStdTupleType <- [t| HsStdTuple |]
  hsTRaw <- hsType
  let
    (hsT, tupleType) = case hsTRaw of
      (AppT a b) | a == hsStdTupleType -> (b, hsType)
      _ -> (hsTRaw, [t| HsStdTuple $hsType |])
    (numTypes, hsTypes) = unfoldTupleT hsT

  unless (numTypes > 0) $
    fail "StdTuple must be a tuple with 1 or more underlying types"

  hsTypesDestructible <-
    forM hsTypes $ \t -> isInstance (mkName "Destructible") [t]

  addressableInst <- instanceD
    (cxt [])
    [t| Addressable $tupleType |]
    []

  let
    sizeOfFn = funD (mkName "sizeOf")
      [ clause [wildP] (normalB $ litE $ integerL $ fromIntegral sizeVal) [] ]

    alignmentFn = funD (mkName "alignment")
      [ clause [wildP] (normalB $
          litE $ integerL $ fromIntegral sizeVal) []
      ]

    ptrN = mkName "ptr"
    pNames = take numTypes $ idxNames "p"
    vNames = take numTypes $ idxNames "v"
    pvZipped = zip pNames vNames
    pCasted = map (\p -> parensE $ varE castPtrN `appE` varE p) pNames

    hsPokeName = mkName $ "c_poke_" ++ cppName
    cppPokeName = "std_tuple_poke_" ++ cppName

    nestedPokeFns = nestPokeFns (zip3 vNames pNames hsTypesDestructible) $ doE
      -- c_poke* FFI call
      [ noBindS (foldl' appE
          (varE hsPokeName `appE` [| castPtr ptr|] `appE` [|nullPtr|])
          pCasted
        )
      ]

    pokeFn = funD (mkName "poke")
      [ clause [varP ptrN, conP (mkName "HsStdTuple") [tupP (map varP vNames)]]
          (normalB nestedPokeFns) []
      ]

    hsPeekName = mkName $ "c_peek_" ++ cppName
    cppPeekName = "std_tuple_peek_" ++ cppName

    nestedPeekFns = nestPeekFns (zip pNames hsTypesDestructible) $
      doE $
        -- c_peek* FFI call
        [noBindS (foldl' appE
            (varE hsPeekName `appE` [| castPtr ptr|] `appE` [|nullPtr|])
            pCasted
          )
        ] ++
        -- N peeks
        map (\(p, v) -> bindS (varP v) (varE (mkName "peek") `appE` varE p))
          pvZipped ++
        -- tuple construction
        [noBindS (varE (mkName "return") `appE`
          parensE (conE (mkName "HsStdTuple") `appE` tupE (map varE vNames)))
        ]

    peekFn = funD (mkName "peek")
      [ clause [varP ptrN] (normalB nestedPeekFns) []
      ]

  storableInst <- instanceD
    (cxt [])
    [t| Storable $tupleType |]
    [sizeOfFn, alignmentFn, pokeFn, peekFn]

  peekImport <- forImpD cCall unsafe cppPeekName hsPeekName $
    -- Ptr () -> Ptr () -> <<< all tuple types >>> -> IO ()
    foldl' (\b a -> [t| $a -> $b |]) [t| IO () |] $
      map (\a -> [t| Ptr $a |]) ([t|()|] : [t| ()|] : map return hsTypes)

  pokeImport <- forImpD cCall unsafe cppPokeName hsPokeName $
    -- Ptr () -> Ptr () -> <<< all tuple types >>> -> IO ()
    foldl' (\b a -> [t| $a -> $b |]) [t| IO () |] $
      map (\a -> [t| Ptr $a |]) ([t|()|] : [t| ()|] : map return hsTypes)

  constructibleInst <- instanceD
    (cxt [])
    [t| Constructible $tupleType |]
    []

  return
    [ addressableInst
    , storableInst
    , constructibleInst
    , peekImport
    , pokeImport
    ]
  where
    unfoldTupleT :: Type -> (Int, [Type])
    unfoldTupleT (AppT a b) = let
      (i, ts) = unfoldTupleT a
      in (i, ts ++ [b])
    unfoldTupleT (TupleT i) = (i, [])
    unfoldTupleT n = (1, [n])

    castPtrN = mkName "castPtr"
    cppName = cppType

    idxNames c = map (\i -> mkName (c ++ show i)) [0::Int ..]

    nestPeekFns :: [(Name, Bool)] -> ExpQ -> ExpQ
    nestPeekFns [] base = base
    nestPeekFns ((p, isDestructible):ts) base =
      varE (mkName allocFn) `appE` lamE [varP p] (nestPeekFns ts base)
      where
        allocFn = if isDestructible then "withDefaultCxxObject" else "alloca"

    nestPokeFns :: [(Name, Name, Bool)] -> ExpQ -> ExpQ
    nestPokeFns [] base = base
    nestPokeFns ((v, p, isDestructible):ts) base =
      varE (mkName withObjFn) `appE` varE v `appE`
        lamE [varP p] (nestPokeFns ts base)
      where
        withObjFn = if isDestructible then "withCxxObject" else "with"
