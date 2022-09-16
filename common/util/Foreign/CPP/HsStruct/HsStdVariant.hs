-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TemplateHaskell #-}

module Foreign.CPP.HsStruct.HsStdVariant
  ( deriveHsStdVariantUnsafe
  ) where

import Language.Haskell.TH

import Foreign
import Foreign.C.Types
import Foreign.CPP.Addressable
import Foreign.CPP.Marshallable

deriveHsStdVariantUnsafe
  :: String -> Int -> Int -> String -> TypeQ -> Q [Dec]
deriveHsStdVariantUnsafe cppName sizeVal alignmentVal hsName hsType = do
  typeName <- do
    v <- lookupTypeName hsName
    case v of
      Just val -> return val
      Nothing -> fail ("Unable to find type " ++ hsName)
  -- Get the constructors from the data type
  constructors <- do
    info <- reify typeName
    case info of
      -- start zip from `1` since `0` corresponds to `monostate`
      TyConI (DataD _ _ _ _ cons' _) -> return $ zip [1..] cons'
      _ -> fail ("Unsupported type for HsStdVariant " ++ hsName)

  addressableInst <- instanceD
    (cxt [])
    [t| Addressable $hsType |]
    []

  let (cppPeekValName, hsPeekValName) = importNames "peek"
  peekValImport <- forImpD cCall unsafe cppPeekValName hsPeekValName
    [t| Ptr () -> Ptr CInt -> IO (Ptr ()) |]

  let (cppPokeValName, hsPokeValName) = importNames "poke"
  pokeValImport <- forImpD cCall unsafe cppPokeValName hsPokeValName
    [t| Ptr () -> Ptr () -> CInt -> IO () |]

  (peekMatches, pokeClauses) <-
    unzip <$> mapM (getConData hsPokeValName) constructors

  -- Storable
  let
    sizeOfFn = funD (mkName "sizeOf")
      [ clause [wildP] (normalB $ litE $ integerL $ fromIntegral sizeVal) [] ]

    alignmentFn = funD (mkName "alignment")
      [ clause [wildP] (normalB $
          litE $ integerL $ fromIntegral alignmentVal) []
      ]

    pokeFn = funD (mkName "poke") pokeClauses

    idxN = mkName "idx"
    idxpN = mkName "idx_p"
    peekN = mkName "peek"

    -- Peek by calling `hsPeekValName` to extract idx and ptr
    -- Unknown returned index results in `error`
    peekFn = funD peekN
      [clause [varP pN]
        (normalB $ varE (mkName "alloca") `appE` lamE [varP idxpN] (doE
          [ bindS
            (varP valpN)
            (varE hsPeekValName `appE`
              parensE (varE castPtrN `appE` varE pN) `appE`
              varE idxpN)
          , bindS (varP idxN) (varE peekN `appE` varE idxpN)
          , noBindS (caseE (varE idxN) (peekMatches ++
              [ match wildP (normalB $
                varE (mkName "error") `appE` parensE
                  (infixApp
                    (stringE ("Unable to peek " ++ hsName ++ " with index "))
                    (varE $ mkName "++")
                    (varE (mkName "show") `appE` varE idxN)
                  )
                ) []
              ])
            )
          ])) []
      ]

  storableInst <- instanceD
    (cxt [])
    [t| Storable $hsType |]
    [sizeOfFn, alignmentFn, pokeFn, peekFn]


  -- Constructible
  constructibleInst <- instanceD
    (cxt [])
    [t| Constructible $hsType |]
    []

  return
    [ addressableInst
    , constructibleInst
    , peekValImport
    , pokeValImport
    , storableInst
    ]
  where
    pN = mkName "p"
    castPtrN = mkName "castPtr"
    valpN = mkName "__val_p"

    importNames pre = ("std_variant_" ++ base, mkName $ "c_" ++ base)
      where
        base = pre ++ cppName

    getConData hsPokeName (idx, NormalC name [(_, t)] ) = do
        isDestructible <- isInstance (mkName "Destructible") [t]

        let
          -- A match clause for the given index, using the constructor `name`
          -- after `peek`ing the result
          peekMatch = match (litP $ integerL idx) (normalB $ infixApp
              (conE name)
              (varE $ mkName "fmap")
              (varE (mkName "peek") `appE` parensE (varE castPtrN `appE`
                varE valpN))
            )
            []

        let
          valN = mkName "__val"
          withObjFn = if isDestructible then "withCxxObject" else "with"
          -- Matching against the particular constructor `name`,
          -- Using with/withCxxObject to get a pointer to the underlying data
          -- to send down
          pokeClause = clause [varP pN, conP name [varP valN]] (normalB $
            varE (mkName withObjFn) `appE` varE valN `appE` lamE [varP valpN] (
              varE hsPokeName `appE`
              parensE (varE castPtrN `appE` varE pN) `appE`
              parensE (varE castPtrN `appE` varE valpN) `appE`
              litE (integerL idx)
            ))
            []

        return (peekMatch, pokeClause)
    getConData _ _ = fail "Only take unnamed constructors with 1 type"
