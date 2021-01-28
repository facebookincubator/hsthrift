-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.CPP.HsStruct.Types
  ( StorableContainer(..)
  -- * HsRange
  , HsRange(..)
  , HsStringPiece
  -- * HsMaybe
  , HsMaybe(..)
  -- * HsEither
  , HsEither(..)
  -- * HsPair
  , HsPair(..)
  -- * HsString
  , HsString(..)
  , HsByteString(..)
  , HsText(..)
  , HsLenientText(..)
  -- * HsArray
  , HsArray(..)
  , HsList(..)
  -- * HsMap
  , HsMap(..)
  , HsIntMap(..)
  , HsHashMap(..)
  , HsObject(..)
  -- * HsJSON
  , HsJSON(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (Value(..))
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Scientific (fromFloatDigits)
import Data.Vector (Vector, generateM)
import Foreign hiding (void)
import Foreign.C
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Foreign.CPP.Addressable hiding (alignment, sizeOf)
import qualified Foreign.CPP.Addressable as Addressable
import Foreign.CPP.Marshallable.TH
import Mangle.TH
import Util.Text (cStringLenToText, cStringLenToTextLenient)

#include <hsc.h>
#include <cpp/HsStruct.h>

-- | An abstraction over storable containers. The elements will be marshaled or
-- interpreted by given function. The element type must be 'Addressable'
-- (with 'sizeOf' and 'alignment' defined) in order to use 'peekWith' and
-- 'pokeWith'.
--
-- Compared with creating a newtype for the element type, it will avoid
--  * duplicate identical implementation of 'sizeOf' and 'pokeWith'
--  * a trivial newtype unwrapping
--  * an unsafe 'castPtr' in some cases
-- while introduce some risk of using wrong marshalers.
class StorableContainer f where
  -- | Given a function to read each element, reads the whole container from
  -- the specified memory location
  peekWith :: Addressable a => (Ptr a -> IO b) -> Ptr (f a) -> IO (f b)
  -- | Given a function to write each element, writes the whole container to
  -- the specified memory location
  pokeWith
    :: Addressable a => (Ptr a -> b -> IO ()) -> Ptr (f a) -> f b -> IO ()

-- The default way to read and write a container. Require 'peek'/'poke' for
-- element type being implemented to use 'peek'/'poke' for the whole container.
instance {-# OVERLAPPABLE #-}
    (Addressable1 f, StorableContainer f, Addressable a, Storable a) =>
    Storable (f a) where
  sizeOf = sizeOf1
  alignment = alignment1
  peek = peekWith peek
  poke = pokeWith poke

peekElemOffWith :: forall a b. Addressable a
                => (Ptr a -> IO b) -> Ptr a -> Int -> IO b
peekElemOffWith f p i = f $ p `plusPtr` (i * offset)
  where
  offset = Addressable.sizeOf (undefined :: a)

notPokeable :: String -> a
notPokeable t = error $ "HsStruct." <> t <> " is not HS_POKEABLE"

-- HsRange --------------------------------------------------------------------

data HsRange a n = HsRange (Ptr a) n

instance Integral n => Storable (HsRange a n) where
  sizeOf _ = #{size DummyHsRange}
  alignment _ = #{alignment DummyHsRange}
  peek p = do
    (a :: Ptr a) <- #{peek DummyHsRange, a} p
    (n :: CSize) <- #{peek DummyHsRange, n} p
    return $ HsRange a $ fromIntegral n
  poke p (HsRange a n) = do
    #{poke DummyHsRange, a} p a
    #{poke DummyHsRange, n} p (fromIntegral n :: CSize)

type HsStringPiece = HsRange CChar Int

-- HsMaybe --------------------------------------------------------------------

newtype HsMaybe a = HsMaybe
  { hsMaybe :: Maybe a
  }

instance Addressable1 HsMaybe where
  sizeOf1 _ = #{size DummyHsMaybe}
  alignment1 _ = #{alignment DummyHsMaybe}

instance Addressable (HsMaybe a) where
  sizeOf = sizeOf1
  alignment = alignment1

instance StorableContainer HsMaybe where
  pokeWith = notPokeable "HsMaybe"
  peekWith f p = do
    ptr <- #{peek DummyHsMaybe, ptr} p
    HsMaybe <$> maybePeek f ptr

-- HsString -------------------------------------------------------------------

newtype HsString = HsString
  { hsString :: String
  }

$(mangle
  "void ctorHsString(HsString*, const char*, size_t)}"
  [d|
    foreign import ccall unsafe
      c_constructHsString :: Ptr HsString -> CString -> Word -> IO ()
  |])

$(mangle
  "HsString* newHsString(const char*, size_t)}"
  [d|
    foreign import ccall unsafe
      c_newHsString :: CString -> Word -> IO (Ptr HsString)
  |])

instance Addressable HsString

instance Storable HsString where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = notPokeable "HsString"
  peek p = fmap HsString $ peekCStringLen =<< peekStrLen p

peekStrLen :: Ptr a -> IO CStringLen
peekStrLen p = do
  (str :: CString) <- #{peek HsString, str} p
  (len :: CSize) <- #{peek HsString, len} p
  return (str, fromIntegral len)

newStrImpl :: ByteString -> IO (Ptr a)
newStrImpl bs = castPtr <$> (unsafeUseAsCStringLen bs $ \(str, len) ->
  c_newHsString str (fromIntegral len))

constructStrImpl :: Ptr a -> ByteString -> IO ()
constructStrImpl p bs = unsafeUseAsCStringLen bs $ \(str, len) ->
  c_constructHsString (castPtr p) str (fromIntegral len)

-- HsText ---------------------------------------------------------------

newtype HsText = HsText
  { hsText :: Text
  }

instance Addressable HsText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}

instance Constructible HsText where
  newValue (HsText txt) = newStrImpl (Text.encodeUtf8 txt)
  constructValue p (HsText txt) =
    constructStrImpl (castPtr p) (Text.encodeUtf8 txt)

instance Assignable HsText

instance Storable HsText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = error "HsStruct.HsText: poke not implemented"
  peek p = fmap HsText $ uncurry cStringLenToText =<< peekStrLen p

-- HsEither -------------------------------------------------------------------

#{enum HsEitherTag, HsEitherTag
  , hs_Left = HS_LEFT
  , hs_Right = HS_RIGHT
  }

newtype HsEitherTag = HsEitherTag #{type HsEitherTag}
  deriving (Eq, Storable)

newtype HsEither a b = HsEither
  { hsEither :: Either a b
  } deriving NFData

$(mangle
  "void* newHsEither(HsEitherTag, void*)}"
  [d|
    foreign import ccall unsafe
      c_newHsEither :: HsEitherTag -> Ptr c -> IO (Ptr (HsEither a b))
  |])

instance Addressable1 (HsEither a) where
  sizeOf1 _ = #{size DummyHsEither}
  alignment1 _ = #{alignment DummyHsEither}

instance Addressable (HsEither a b) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable a, Storable a) => StorableContainer (HsEither a) where
  pokeWith = notPokeable "HsEither"
  peekWith f p = do
    isLeft <- #{peek DummyHsEither, isLeft} p :: IO CChar
    if toBool isLeft
    then do
      ptr <- #{peek DummyHsEither, left} p
      HsEither . Left <$> peek ptr
    else do
      ptr <- #{peek DummyHsEither, right} p
      HsEither . Right <$> f ptr

instance (Constructible a, Constructible b)
  => Constructible (HsEither a b) where
  newValue (HsEither (Left a)) = do
    p_left <- newValue a
    c_newHsEither hs_Left p_left
  newValue (HsEither (Right b)) = do
    p_right <- newValue b
    c_newHsEither hs_Right p_right

  constructValue p (HsEither (Left a)) = do
    p_left <- newValue a
    #{poke DummyHsEither, isLeft} p (fromIntegral (1::Int) :: CBool)
    #{poke DummyHsEither, left} p p_left
  constructValue p (HsEither (Right b)) = do
    #{poke DummyHsEither, isLeft} p (fromIntegral (0::Int) :: CBool)
    p_right <- newValue b
    #{poke DummyHsEither, right} p p_right

instance Assignable (HsEither HsText Int)
instance Assignable (HsEither HsText Double)
instance Assignable (HsEither HsText HsByteString)
instance Assignable (HsEither HsText HsText)

-- HsPair ---------------------------------------------------------------------

newtype HsPair a b = HsPair
  { hsPair :: (a, b)
  }

instance Addressable1 (HsPair a) where
  sizeOf1 _ = #{size DummyHsPair}
  alignment1 _ = #{alignment DummyHsPair}

instance Addressable (HsPair a b) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable a, Storable a) => StorableContainer (HsPair a) where
  pokeWith = notPokeable "HsPair"
  peekWith f p = do
    ptr_a <- #{peek DummyHsPair, fst_} p
    fs <- peek ptr_a
    ptr_b <- #{peek DummyHsPair, snd_} p
    sn <- f ptr_b
    return $ HsPair (fs, sn)

-- HsByteString ---------------------------------------------------------------

newtype HsByteString = HsByteString
  { hsByteString :: ByteString
  } deriving NFData

instance Addressable HsByteString where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}

instance Constructible HsByteString where
  newValue (HsByteString bs) = newStrImpl bs
  constructValue p (HsByteString bs) = constructStrImpl (castPtr p) bs

instance Assignable HsByteString

instance Storable HsByteString where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = notPokeable "HsByteString"
  peek p = fmap HsByteString $ packCStringLen =<< peekStrLen p

-- HsLenientText --------------------------------------------------------------

newtype HsLenientText = HsLenientText
  { hsLenientText :: Text
  }

instance Addressable HsLenientText

instance Storable HsLenientText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = notPokeable "HsLenientText"
  peek p = fmap HsLenientText $ uncurry cStringLenToTextLenient =<< peekStrLen p

-- HsArray
newtype HsArray a = HsArray
  { hsArray :: Vector a
  }

instance Addressable1 HsArray where
  sizeOf1 _ = #{size DummyHsArray}
  alignment1 _ = #{alignment DummyHsArray}

instance Addressable (HsArray a) where
  sizeOf = sizeOf1
  alignment = alignment1

instance StorableContainer HsArray where
  pokeWith = notPokeable "HsArray"
  peekWith f p = do
    (a, n) <- peekDataSize p
    fmap HsArray . generateM n $ \i -> peekElemOffWith f a i

newtype HsList a = HsList
  { hsList :: [a]
  }

instance Addressable1 HsList where
  sizeOf1 _ = #{size DummyHsArray}
  alignment1 _ = #{alignment DummyHsArray}

instance Addressable (HsList a) where
  sizeOf = sizeOf1
  alignment = alignment1

instance StorableContainer HsList where
  pokeWith = notPokeable "HsList"
  peekWith f p = do
    (a, n) <- peekDataSize p
    let go !i !acc
          | i < 0 = return acc
          | otherwise = do
              e <- peekElemOffWith f a i
              go (i-1) (e:acc)
    HsList <$> go (n-1) []

peekDataSize :: Ptr b -> IO (Ptr a, Int)
peekDataSize p = do
  (a :: Ptr a) <- #{peek DummyHsArray, a} p
  (n :: CSize) <- #{peek DummyHsArray, n} p
  return (a, fromIntegral n)

-- HsMap
newtype HsMap k v = HsMap
  { hsMap :: Map k v
  }

instance Addressable1 (HsMap k) where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsMap k v) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable k, Ord k, Storable k) => StorableContainer (HsMap k) where
  pokeWith = notPokeable "HsMap"
  peekWith f p = HsMap <$> peekMapWith Map.empty Map.insert peek f p

newtype HsIntMap v = HsIntMap
  { hsIntMap :: IntMap v
  }

instance Addressable1 HsIntMap where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsIntMap v) where
  sizeOf = sizeOf1
  alignment = alignment1

instance StorableContainer HsIntMap where
  pokeWith = notPokeable "HsIntMap"
  peekWith f p = HsIntMap <$> peekMapWith IntMap.empty IntMap.insert peek f p

newtype HsHashMap k v = HsHashMap
  { hsHashMap :: HashMap k v
  }

instance Addressable1 (HsHashMap k) where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsHashMap k v) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable k, Eq k, Hashable k, Storable k) =>
    StorableContainer (HsHashMap k) where
  pokeWith = notPokeable "HsHashMap"
  peekWith f p = HsHashMap <$> peekMapWith HashMap.empty HashMap.insert peek f p

newtype HsObject v = HsObject
  { hsObject :: HashMap Text v
  }

instance Addressable1 HsObject where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsObject v) where
  sizeOf = sizeOf1
  alignment = alignment1

instance StorableContainer HsObject where
  pokeWith = notPokeable "HsObject"
  peekWith f p = HsObject <$> peekMapWith HashMap.empty HashMap.insert fk f p
    where
    fk = fmap hsText . peek

mapSizeOf, mapAlignment :: a -> Int
mapSizeOf _ = #{size DummyHsObject}
mapAlignment _ = #{alignment DummyHsObject}

{-# INLINE peekMapWith #-}
peekMapWith
  :: (Addressable a, Addressable b)
  => m
  -> (k -> v -> m -> m)
  -> (Ptr a -> IO k)
  -> (Ptr b -> IO v)
  -> Ptr c
  -> IO m
peekMapWith empty insert fk fv p = do
  size <- (fromIntegral :: CSize -> Int) <$> #{peek DummyHsObject, n} p
  keys <- #{peek DummyHsObject, keys} p
  values <- #{peek DummyHsObject, values} p
  let go !i !hm
        | i >= size = return hm
        | otherwise = do
            k <- peekElemOffWith fk keys i
            v <- peekElemOffWith fv values i
            go (i+1) $ insert k v hm
  go 0 empty

-- HsJSON
newtype HsJSON = HsJSON
  { hsJSON :: Value
  }

instance Addressable HsJSON

instance Storable HsJSON where
  sizeOf _ = #{size HsJSON}
  alignment _ = #{alignment HsJSON}
  poke = notPokeable "HsJSON"
  peek p = do
    (t :: CInt) <- #{peek HsJSON, type} p
    #{let json_type t = "%d", static_cast<int>(HsJSON::Type::t)}
    HsJSON <$> case t of
      #{json_type Null} ->
        return Null
      #{json_type Bool} ->
        Bool . toBool <$> peekIntegral
      #{json_type Integral} ->
        Number . fromIntegral <$> peekIntegral
      #{json_type Real} ->
        Number . fromFloatDigits <$> (#{peek HsJSON, real} p :: IO CDouble)
      #{json_type String} ->
        String . hsText <$> #{peek HsJSON, string} p
      #{json_type Array} ->
        Array . fmap hsJSON . hsArray <$> #{peek HsJSON, array} p
      #{json_type Object} ->
        Object . fmap hsJSON . hsObject <$> #{peek HsJSON, object} p
      _ ->
        error "HsStruct.HsJSON: invalid HsJSON::Type"
    where
    peekIntegral = #{peek HsJSON, integral} p :: IO CLong

$(deriveMarshallableUnsafe "HsMaybeInt" [t| HsMaybe Int |])
$(deriveMarshallableUnsafe "HsMaybeDouble" [t| HsMaybe Double |])
$(deriveMarshallableUnsafe "HsMaybeString" [t| HsMaybe HsByteString |])
$(deriveMarshallableUnsafe "HsMaybeString" [t| HsMaybe HsText |])

$(deriveMarshallableUnsafe "HsEitherStringInt" [t| HsEither HsText Int |])
$(deriveMarshallableUnsafe "HsEitherStringDouble" [t| HsEither HsText Double |])
$(deriveMarshallableUnsafe "HsEitherStringString" [t| HsEither HsText HsByteString |])
$(deriveMarshallableUnsafe "HsEitherStringString" [t| HsEither HsText HsText |])

$(deriveMarshallableUnsafe "HsEitherStringArrayInt" [t| HsEither HsText (HsList Int) |])
$(deriveMarshallableUnsafe "HsEitherStringArrayDouble" [t| HsEither HsText (HsList Double) |])
$(deriveMarshallableUnsafe "HsEitherStringArrayString" [t| HsEither HsText (HsList HsByteString) |])
$(deriveMarshallableUnsafe "HsEitherStringArrayString" [t| HsEither HsText (HsList HsText) |])

$(deriveMarshallableUnsafe "HsString" [t| HsString |])
$(deriveMarshallableUnsafe "HsString" [t| HsByteString |])
$(deriveMarshallableUnsafe "HsString" [t| HsText |])
$(deriveMarshallableUnsafe "HsString" [t| HsLenientText |])

$(deriveMarshallableUnsafe "HsArrayInt" [t| HsList Int |])
$(deriveMarshallableUnsafe "HsArrayDouble" [t| HsList Double |])
$(deriveMarshallableUnsafe "HsArrayString" [t| HsList HsByteString |])
$(deriveMarshallableUnsafe "HsArrayString" [t| HsList HsText |])

$(deriveMarshallableUnsafe "HsMapIntInt" [t| HsIntMap Int |])
$(deriveMarshallableUnsafe "HsMapIntDouble" [t| HsIntMap Double |])
$(deriveMarshallableUnsafe "HsMapIntString" [t| HsIntMap HsByteString |])
$(deriveMarshallableUnsafe "HsMapIntString" [t| HsIntMap HsText |])
$(deriveMarshallableUnsafe "HsMapStringInt" [t| HsObject Int |])
$(deriveMarshallableUnsafe "HsMapStringDouble" [t| HsObject Double |])
$(deriveMarshallableUnsafe "HsMapStringString" [t| HsObject HsByteString |])
$(deriveMarshallableUnsafe "HsMapStringString" [t| HsObject HsText |])

$(deriveMarshallableUnsafe "HsJSON" [t| HsJSON |])
