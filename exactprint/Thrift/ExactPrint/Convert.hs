-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP #-}
module Thrift.ExactPrint.Convert
  ( computeOffsets
  , computeThriftFileOffsets
  , declOffsets
  ) where

import Prelude hiding (Enum)
import Data.Bifunctor
import Data.List
import Data.Some
import Unsafe.Coerce

import Thrift.Compiler.Parser
import Thrift.Compiler.Types

import Thrift.ExactPrint.Types

#if MIN_VERSION_dependent_sum(0,6,0)
#define This Some
#endif

computeOffsets :: Program l Loc -> Program l Offset
computeOffsets Program{..} = Program
  { progIncludes = map computeOffsets progIncludes
  , progHeaders  = headers
  , progDecls    = decls
  , progComments = comments
  , ..
  }
  where
    (headers, headerEnd) = foldO computeHeaderOffsets absoluteOrigin progHeaders
    (decls, declsEnd) = foldO computeDeclOffsets headerEnd progDecls
    (comments, _) = foldO commentOffset declsEnd progComments

computeThriftFileOffsets :: ThriftFile a Loc -> ThriftFile a Offset
computeThriftFileOffsets ThriftFile{..} = ThriftFile
  { thriftHeaders = headers
  , thriftDecls = decls
  , thriftComments = comments
  , ..
  }
  where
    (headers, headerEnd) =
      foldO computeHeaderOffsets absoluteOrigin thriftHeaders
    (decls, declsEnd) = foldO computeDeclOffsets headerEnd thriftDecls
    (comments, _) = foldO commentOffset declsEnd thriftComments

declOffsets :: [Decl s l Loc] -> [Decl s l Offset]
declOffsets = fst . foldO computeDeclOffsets absoluteOrigin

absoluteOrigin :: Loc
absoluteOrigin = Loc
  { locFile = ""
  , locStartLine = 1
  , locStartCol  = 1
  , locEndLine   = 1
  , locEndCol    = 1
  }

-- Headers ---------------------------------------------------------------------

computeHeaderOffsets :: Loc -> Header Loc -> (Header Offset, Loc)
computeHeaderOffsets origin HInclude{..} =
  (HInclude { incKeywordLoc = getOffsets origin incKeywordLoc
            , incPathLoc    = getOffsets (lLocation incKeywordLoc) incPathLoc
            , ..
            },
   lLocation incPathLoc)
computeHeaderOffsets origin HNamespace{..} =
  (HNamespace { nmKeywordLoc = getOffsets origin nmKeywordLoc
              , nmLangLoc    = getOffsets (lLocation nmKeywordLoc) nmLangLoc
              , nmNameLoc    = getOffsets (lLocation nmLangLoc) nmNameLoc
              , ..
              },
   lLocation nmNameLoc)

-- Decls -----------------------------------------------------------------------

computeDeclOffsets :: Loc -> Decl s l Loc -> (Decl s l Offset, Loc)
computeDeclOffsets origin d = case d of
  D_Struct s  -> first D_Struct $ computeStructOffsets origin s
  D_Union u   -> first D_Union $ computeUnionOffsets origin u
  D_Enum e    -> first D_Enum $ computeEnumOffsets origin e
  D_Typedef t -> first D_Typedef $ computeTypedefOffsets origin t
  D_Const c   -> first D_Const $ computeConstOffsets origin c
  D_Service s -> first D_Service $ computeServiceOffsets origin s

-- Structs ---------------------------------------------------------------------

computeStructOffsets :: Loc -> Struct s l Loc -> (Struct s l Offset, Loc)
computeStructOffsets origin Struct{..} =
  (Struct { structMembers = fields
          , structLoc = offsets
          , structAnns = anns
          , structSAnns = sAnns
          , ..
          },
   structEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin structSAnns
    (offsets, anns, structEnd) =
      computeStructLoc sAnnsEnd structLoc structAnns fieldEnd
    (fields, fieldEnd) =
      foldO computeFieldOffsets (lLocation slOpenBrace) structMembers
    StructLoc{..} = structLoc

computeStructLoc
  :: Loc
  -> StructLoc Loc
  -> Maybe (Annotations Loc)
  -> Loc
  -> (StructLoc Offset, Maybe (Annotations Offset), Loc)
computeStructLoc origin StructLoc{..} anns fieldEnd =
  (StructLoc
   { slKeyword    = getOffsets origin slKeyword
   , slName       = getOffsets (lLocation slKeyword) slName
   , slOpenBrace  = getOffsets (lLocation slName) slOpenBrace
   , slCloseBrace = getOffsets fieldEnd slCloseBrace
   },
   anns',
   end)
  where
    (anns', end) = annsOffsets (lLocation slCloseBrace) anns

-- Unions ----------------------------------------------------------------------

computeUnionOffsets :: Loc -> Union s l Loc -> (Union s l Offset, Loc)
computeUnionOffsets origin Union{..} =
  (Union { unionAlts = alts
         , unionLoc = offsets
         , unionAnns = anns
         , unionSAnns = sAnns
         , ..
         },
   unionEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin unionSAnns
    (offsets, anns, unionEnd) =
      computeStructLoc sAnnsEnd unionLoc unionAnns altsEnd
    (alts, altsEnd) =
      foldO computeAltOffsets (lLocation slOpenBrace) unionAlts
    StructLoc{..} = unionLoc

computeAltOffsets :: Loc -> UnionAlt s l Loc -> (UnionAlt s l Offset, Loc)
computeAltOffsets origin UnionAlt{..} =
  (UnionAlt
   { altType = ty
   , altLoc = FieldLoc
     { flId        = getOffsets sAnnsEnd flId
     , flIdRep     = flIdRep
     , flColon     = getOffsets (lLocation flId) flColon
     , flName      = getOffsets tyEnd flName
     , flEqual     = Nothing
     , flSeparator = sep
     }
   , altAnns = anns
   , altSAnns = sAnns
   , altResolvedType = unsafeCoerce altResolvedType
   , ..
   },
   sepEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin altSAnns
    (ty, tyEnd) = computeTypeOffsets (lLocation flColon) altType
    (anns, annsEnd) = annsOffsets (lLocation flName) altAnns
    (sep, sepEnd) = separatorOffsets annsEnd flSeparator
    FieldLoc{..} = altLoc

-- Fields ----------------------------------------------------------------------

computeFieldOffsets
  :: Loc
  -> Field u s l Loc
  -> (Field u s l Offset, Loc)
computeFieldOffsets origin Field{..} =
  (Field
    { fieldVal = fst <$> defVal
    , fieldRequiredness = req
    , fieldLoc = FieldLoc
      { flId        = getOffsets sAnnsEnd flId
      , flIdRep     = flIdRep
      , flColon     = getOffsets (lLocation flId) flColon
      , flName      = getOffsets tyEnd flName
      , flEqual     = getOffsets (lLocation flName) <$> flEqual
      , flSeparator = sep
      }
    , fieldType = ty
    , fieldAnns = anns
    , fieldSAnns = sAnns
    , ..
    },
   sepEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin fieldSAnns
    (req, reqEnd) = case fieldRequiredness of
      Default -> (Default, lLocation flColon)
      Required loc ->
        (Required $ getOffsets (lLocation flColon) loc, lLocation loc)
      Optional loc ->
        (Optional $ getOffsets (lLocation flColon) loc, lLocation loc)
    (ty, tyEnd) = computeTypeOffsets reqEnd fieldType
    defVal = constOffsets . lLocation <$> flEqual <*> fieldVal
    fend = maybe (lLocation flName) snd defVal
    (anns, annsEnd) = annsOffsets fend fieldAnns
    (sep, sepEnd) = separatorOffsets annsEnd flSeparator
    FieldLoc{..} = fieldLoc

-- Enums -----------------------------------------------------------------------

computeEnumOffsets :: Loc -> Enum s l Loc -> (Enum s l Offset, Loc)
computeEnumOffsets origin Enum{..} =
  (Enum { enumConstants = consts
        , enumLoc = offsets
        , enumAnns = anns
        , enumSAnns = sAnns
        , ..
        },
   enumEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin enumSAnns
    (offsets, anns, enumEnd) =
      computeStructLoc sAnnsEnd enumLoc enumAnns constEnd
    (consts, constEnd) =
      foldO enumValOffsets (lLocation slOpenBrace) enumConstants
    StructLoc{..} = enumLoc

enumValOffsets :: Loc -> EnumValue s l Loc -> (EnumValue s l Offset, Loc)
enumValOffsets origin EnumValue{..} =
  (EnumValue
    { evLoc = EnumValLoc
      { evlName      = getOffsets sAnnsEnd evlName
      , evlEqual     = getOffsets (lLocation evlName) evlEqual
      , evlValue     = getOffsets (lLocation evlEqual) evlValue
      , evlRep       = evlRep
      , evlSeparator = sep
      }
    , evAnns = anns
    , evSAnns = sAnns
    , ..
    },
   sepEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin evSAnns
    (anns, annsEnd) = annsOffsets (lLocation evlValue) evAnns
    (sep, sepEnd) = separatorOffsets annsEnd evlSeparator
    EnumValLoc{..} = evLoc

-- Typedefs --------------------------------------------------------------------

computeTypedefOffsets :: Loc -> Typedef s l Loc -> (Typedef s l Offset, Loc)
computeTypedefOffsets origin Typedef{..} =
  (Typedef
    { tdType = ty
    , tdLoc = TypedefLoc
      { tdlKeyword = getOffsets sAnnsEnd tdlKeyword
      , tdlName = getOffsets tyEnd tdlName
      }
    , tdAnns = anns
    , tdResolvedType = unsafeCoerce tdResolvedType
    , tdSAnns = sAnns
    , ..
    },
   annsEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin tdSAnns
    (ty, tyEnd) = computeTypeOffsets (lLocation tdlKeyword) tdType
    (anns, annsEnd) = annsOffsets (lLocation tdlName) tdAnns
    TypedefLoc{..} = tdLoc

-- Constants -------------------------------------------------------------------

computeConstOffsets :: Loc -> Const s l Loc -> (Const s l Offset, Loc)
computeConstOffsets origin Const{..} =
  (Const
     { constType = ty
     , constVal = val
     , constLoc = ConstLoc
       { clKeyword = getOffsets sAnnsEnd clKeyword
       , clName    = getOffsets tyEnd clName
       , clEqual   = getOffsets (lLocation clName) clEqual
       , clSeparator = sep
       }
     , constResolvedType = unsafeCoerce constResolvedType
     , constResolvedVal = unsafeCoerce constResolvedVal
     , constSAnns = sAnns
     , ..
     },
   sepEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin constSAnns
    (ty, tyEnd) = computeTypeOffsets (lLocation clKeyword) constType
    (val, valEnd) = constOffsets (lLocation clEqual) constVal
    (sep, sepEnd) = separatorOffsets valEnd clSeparator
    ConstLoc{..} = constLoc

constOffsets :: Loc -> UntypedConst Loc -> (UntypedConst Offset, Loc)
constOffsets origin UntypedConst{..} =
  (UntypedConst
   { ucLoc   = getOffsets origin ucLoc
   , ucConst = c
   },
   lLocation end)
  where
    (c, end) = case ucConst of
      IntConst i r    -> (IntConst i r, ucLoc)
      DoubleConst d r -> (DoubleConst d r, ucLoc)
      StringConst s q -> (StringConst s q, ucLoc)
      IdConst i     -> (IdConst i, ucLoc)
      BoolConst b   -> (BoolConst b, ucLoc)
      ListConst{..} ->
        (ListConst
         { lvElems = elems
         , lvCloseBrace = getOffsets elemsEnd lvCloseBrace
         },
         lvCloseBrace)
        where
          (elems, elemsEnd) =
            foldO (listElemOffsets constOffsets) (lLocation ucLoc)
            lvElems
      MapConst{..} ->
        (MapConst
         { mvElems = elems
         , mvCloseBrace = getOffsets elemsEnd mvCloseBrace
         },
         mvCloseBrace)
        where
          (elems, elemsEnd) =
            foldO (listElemOffsets mapPairOffsets) (lLocation ucLoc) mvElems
      StructConst{..} ->
        (StructConst
         { svType = svType
         , svOpenBrace = openBrace
         , svElems = elems
         , svCloseBrace = getOffsets elemsEnd svCloseBrace
         },
         svCloseBrace)
        where
          openBrace = getOffsets (lLocation ucLoc) svOpenBrace
          (elems, elemsEnd) =
            foldO
              (listElemOffsets structPairOffsets)
              (lLocation svOpenBrace)
              svElems

listElemOffsets
  :: (Loc -> f Loc -> (f Offset, Loc))
  -> Loc
  -> ListElem f Loc
  -> (ListElem f Offset, Loc)
listElemOffsets f origin ListElem{..} =
  (ListElem
   { leElem = el
   , leSeparator = sep
   },
   sepEnd)
  where
    (sep, sepEnd) = separatorOffsets elEnd leSeparator
    (el, elEnd) = f origin leElem

mapPairOffsets :: Loc -> MapPair Loc -> (MapPair Offset, Loc)
mapPairOffsets origin MapPair{..} =
  (MapPair
   { mpKey   = key
   , mpColon = getOffsets keyEnd mpColon
   , mpVal   = val
   },
   valEnd)
  where
    (key, keyEnd) = constOffsets origin mpKey
    (val, valEnd) = constOffsets (lLocation mpColon) mpVal

structPairOffsets :: Loc -> StructPair Loc -> (StructPair Offset, Loc)
structPairOffsets origin StructPair{..} =
  (StructPair
   { spKey    = spKey
   , spKeyLoc = keyLoc
   , spEquals = getOffsets (lLocation spKeyLoc) spEquals
   , spVal    = val
   },
   valEnd)
  where
    keyLoc = getOffsets origin spKeyLoc
    (val, valEnd) = constOffsets (lLocation spEquals) spVal

-- Services --------------------------------------------------------------------

computeServiceOffsets :: Loc -> Service s l Loc -> (Service s l Offset, Loc)
computeServiceOffsets origin Service{..} =
  (Service
   { serviceSuper = super
   , serviceFunctions = funcs
   , serviceLoc = StructLoc
     { slKeyword = getOffsets sAnnsEnd slKeyword
     , slName    = getOffsets (lLocation slKeyword) slName
     , slOpenBrace  = getOffsets superEnd slOpenBrace
     , slCloseBrace = getOffsets funcEnd slCloseBrace
     }
   , serviceAnns = anns
   , serviceSAnns = sAnns
   , ..
   },
   annsEnd)
  where
    (super, superEnd) = case serviceSuper of
      Nothing -> (Nothing, lLocation slName)
      Just Super{..} ->
        (Just Super { supExtends = getOffsets (lLocation slName) supExtends
                    , supLoc     = getOffsets (lLocation supExtends) supLoc
                    , ..
                    },
         lLocation supLoc)
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin serviceSAnns
    (funcs, funcEnd) =
      foldO functionOffsets (lLocation slOpenBrace) serviceFunctions
    (anns, annsEnd) = annsOffsets (lLocation slCloseBrace) serviceAnns
    StructLoc{..} = serviceLoc

functionOffsets :: Loc -> Function s l Loc -> (Function s l Offset, Loc)
functionOffsets origin Function{..} =
  (Function
   { funType = ty
   , funArgs = args
   , funExceptions = exs
   , funLoc = FunLoc
     { fnlOneway     = getOffsets sAnnsEnd <$> fnlOneway
     , fnlName       = getOffsets tyEnd fnlName
     , fnlOpenParen  = getOffsets (lLocation fnlName) fnlOpenParen
     , fnlCloseParen = getOffsets argsEnd fnlCloseParen
     , fnlThrows     = throws
     , fnlSeparator  = sep
     }
   , funAnns = anns
   , funSAnns = sAnns
   , ..
   },
   sepEnd)
  where
    (sAnns, sAnnsEnd) = foldO sAnnOffsets origin funSAnns
    onewayEnd = maybe sAnnsEnd lLocation fnlOneway
    (ty, tyEnd) = case funType of
      Left loc -> (Left $ getOffsets onewayEnd loc, lLocation loc)
      Right (This t) -> first (Right . This) $ computeTypeOffsets onewayEnd t
    (args, argsEnd) = foldO computeFieldOffsets (lLocation fnlOpenParen) funArgs
    (throws, exs, throwsEnd) = case fnlThrows of
      Nothing -> (Nothing, [], lLocation fnlCloseParen)
      Just ThrowsLoc{..} ->
        (Just ThrowsLoc
         { tlThrows     = getOffsets (lLocation fnlCloseParen) tlThrows
         , tlOpenParen  = getOffsets (lLocation tlThrows) tlOpenParen
         , tlCloseParen = getOffsets exEnd tlCloseParen
         },
         ex,
         lLocation tlCloseParen)
        where
          (ex, exEnd) =
            foldO computeFieldOffsets (lLocation tlOpenParen) funExceptions
    (anns, annsEnd) = annsOffsets throwsEnd funAnns
    (sep, sepEnd) = separatorOffsets annsEnd fnlSeparator
    FunLoc{..} = funLoc

-- Types -----------------------------------------------------------------------

computeTypeOffsets
  :: forall t. Loc -> AnnotatedType Loc t -> (AnnotatedType Offset t, Loc)
computeTypeOffsets origin AnnotatedType{..} = (,annsEnd) $ case atType of
  -- Arity 0 Types
  I8  -> arity0Offsets I8 atLoc
  I16 -> arity0Offsets I16 atLoc
  I32 -> arity0Offsets I32 atLoc
  I64 -> arity0Offsets I64 atLoc
  TFloat  -> arity0Offsets TFloat atLoc
  TDouble -> arity0Offsets TDouble atLoc
  TBool   -> arity0Offsets TBool atLoc
  TText   -> arity0Offsets TText atLoc
  TBytes  -> arity0Offsets TBytes atLoc
  TNamed n -> arity0Offsets (TNamed n) atLoc

  -- Arity 1 Types
  TList    u -> arity1Offsets TList u atLoc
  TSet     u -> arity1Offsets TSet u atLoc
  THashSet u -> arity1Offsets THashSet u atLoc

  -- Arity 2 Types
  TMap     k v -> arity2Offsets TMap k v atLoc
  THashMap k v -> arity2Offsets THashMap k v atLoc
  where
    arity0Offsets
      :: GetArity t ~ 0
      => TType 'Unresolved () Offset t
      -> TypeLoc 0 Loc
      -> AnnotatedType Offset t
    arity0Offsets ty Arity0Loc{..} = AnnotatedType
      { atType = ty
      , atLoc = Arity0Loc { a0Ty = getOffsets origin a0Ty }
      , atAnnotations = anns
      }

    arity1Offsets
      :: (GetArity t ~ 1, f a ~ t)
      => (AnnotatedType Offset a -> TType 'Unresolved () Offset (f a))
      -> AnnotatedType Loc a
      -> TypeLoc 1 Loc
      -> AnnotatedType Offset (f a)
    arity1Offsets f a Arity1Loc{..} = AnnotatedType
      { atType = f ty
      , atLoc = Arity1Loc
        { a1Ty         = getOffsets origin a1Ty
        , a1OpenBrace  = getOffsets (lLocation a1Ty) a1OpenBrace
        , a1CloseBrace = getOffsets tyEnd a1CloseBrace
        }
      , atAnnotations = anns
      }
      where
        (ty, tyEnd) = computeTypeOffsets (lLocation a1OpenBrace) a

    arity2Offsets
      :: (GetArity t ~ 2, f a b ~ t)
      => (AnnotatedType Offset a -> AnnotatedType Offset b ->
            TType 'Unresolved () Offset (f a b))
      -> AnnotatedType Loc a
      -> AnnotatedType Loc b
      -> TypeLoc 2 Loc
      -> AnnotatedType Offset (f a b)
    arity2Offsets f a b Arity2Loc{..} = AnnotatedType
      { atType = f a' b'
      , atLoc = Arity2Loc
        { a2Ty         = getOffsets origin a2Ty
        , a2OpenBrace  = getOffsets (lLocation a2Ty) a2OpenBrace
        , a2Comma      = getOffsets aEnd a2Comma
        , a2CloseBrace = getOffsets bEnd a2CloseBrace
        }
      , atAnnotations = anns
      }
      where
        (a', aEnd) = computeTypeOffsets (lLocation a2OpenBrace) a
        (b', bEnd) = computeTypeOffsets (lLocation a2Comma) b

    (anns, annsEnd) = annsOffsets (getTyEnd atLoc) atAnnotations

getTyEnd :: TypeLoc n Loc -> Loc
getTyEnd Arity0Loc{..} = lLocation a0Ty
getTyEnd Arity1Loc{..} = lLocation a1CloseBrace
getTyEnd Arity2Loc{..} = lLocation a2CloseBrace

-- Annotations -----------------------------------------------------------------

annsOffsets
  :: Loc -> Maybe (Annotations Loc) -> (Maybe (Annotations Offset), Loc)
annsOffsets origin Nothing = (Nothing, origin)
annsOffsets origin (Just Annotations{..}) =
  (Just Annotations
   { annList = list
   , annOpenParen  = getOffsets origin annOpenParen
   , annCloseParen = getOffsets listEnd annCloseParen
   },
   lLocation annCloseParen)
  where
    (list, listEnd) = foldO annOffsets (lLocation annOpenParen) annList

annOffsets :: Loc -> Annotation Loc -> (Annotation Offset, Loc)
annOffsets origin SimpleAnn{..} =
  (SimpleAnn
   { saLoc = getOffsets origin saLoc
   , saSep = sep
   , .. },
   sepEnd)
  where
    (sep, sepEnd) = separatorOffsets (lLocation saLoc) saSep
annOffsets origin ValueAnn{..} =
  (ValueAnn
   { vaTagLoc = getOffsets origin vaTagLoc
   , vaEqual  = getOffsets (lLocation vaTagLoc) vaEqual
   , vaValLoc = getOffsets (lLocation vaEqual) vaValLoc
   , vaSep    = sep
   , ..
   },
   sepEnd)
  where
    (sep, sepEnd) = separatorOffsets (lLocation vaValLoc) vaSep

sAnnOffsets
  :: Loc
  -> StructuredAnnotation s l Loc
  -> (StructuredAnnotation s l Offset, Loc)
sAnnOffsets origin StructuredAnn{..} =
  (StructuredAnn
   { saAt = getOffsets origin saAt
   , saMaybeElems = maybeElems
   , saTypeLoc = typeLoc
   , saResolvedType = unsafeCoerce saResolvedType
   , .. },
   elemsEnd)
  where
    (maybeElems, elemsEnd) =
      sAnnElemOffsets (lLocation $ a0Ty saTypeLoc) saMaybeElems
    typeLoc = Arity0Loc { a0Ty = getOffsets (lLocation saAt) (a0Ty saTypeLoc) }

sAnnElemOffsets
  :: Loc
  -> Maybe (StructuredAnnotationElems Loc)
  -> (Maybe (StructuredAnnotationElems Offset), Loc)
sAnnElemOffsets origin Nothing = (Nothing, origin)
sAnnElemOffsets origin (Just StructuredAnnElems{..}) =
  (Just $ StructuredAnnElems
   { saOpenBrace = getOffsets origin saOpenBrace
   , saElems = elems

   , saCloseBrace = getOffsets elemsEnd saCloseBrace
   , .. },
   lLocation saCloseBrace)
  where
    (elems, elemsEnd) =
      foldO
        (listElemOffsets structPairOffsets)
        (lLocation saOpenBrace)
        saElems


-- Helpers ---------------------------------------------------------------------

separatorOffsets :: Loc -> Separator Loc -> (Separator Offset, Loc)
separatorOffsets origin = \case
  NoSep -> (NoSep, origin)
  Semicolon loc -> (Semicolon $ getOffsets origin loc, lLocation loc)
  Comma loc     -> (Comma $ getOffsets origin loc, lLocation loc)

foldO :: (Loc -> a -> (b, Loc)) -> Loc -> [a] -> ([b], Loc)
foldO f origin xs =  first reverse $
  foldl' (\(ys, loc) x -> first (:ys) $ f loc x) ([], origin) xs

getOffsets
  :: Loc
  -> Located Loc
  -> Located Offset
getOffsets origin Located{..} = Located
  { lLocation = getDelta commentsEnd lLocation
  , lComments = comments
  }
  where
    (comments, commentsEnd) = foldO commentOffset origin lComments

commentOffset :: Loc -> Comment Loc -> (Comment Offset, Loc)
commentOffset origin (Comment loc txt) =
  (Comment (getDelta origin loc) txt, loc)

getDelta :: Loc -> Loc -> Offset
getDelta l1 l2 = Offset{..}
  where
    offsRows = locStartLine l2 - locEndLine l1
    offsCols | offsRows > 0 = locStartCol l2 - 1
             | otherwise    = locStartCol l2 - locEndCol l1
