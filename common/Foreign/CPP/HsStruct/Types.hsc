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

import Data.Aeson (Value(..))
import Data.ByteString (ByteString, packCStringLen)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Scientific (fromFloatDigits)
import Data.Vector (Vector, generateM)
import Foreign
import Foreign.C
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Foreign.CPP.Addressable hiding (alignment, sizeOf)
import qualified Foreign.CPP.Addressable as Addressable
import Foreign.CPP.Destructible.TH
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

-- HsRange
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

-- HsMaybe
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
  pokeWith = error "HsStruct.HsMaybe: pokeWith not implemented"
  peekWith f p = do
    ptr <- #{peek DummyHsMaybe, ptr} p
    HsMaybe <$> maybePeek f ptr

-- HsEither
newtype HsEither a b = HsEither
  { hsEither :: Either a b
  }

instance Addressable1 (HsEither a) where
  sizeOf1 _ = #{size DummyHsEither}
  alignment1 _ = #{alignment DummyHsEither}

instance Addressable (HsEither a b) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable a, Storable a) => StorableContainer (HsEither a) where
  pokeWith = error "HsStruct.HsEither: pokeWith not implemented"
  peekWith f p = do
    isLeft <- #{peek DummyHsEither, isLeft} p :: IO CChar
    if toBool isLeft
    then do
      ptr <- #{peek DummyHsEither, left} p
      HsEither . Left <$> peek ptr
    else do
      ptr <- #{peek DummyHsEither, right} p
      HsEither . Right <$> f ptr

-- HsPair
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
  pokeWith = error "HsStruct.HsPair: pokeWith not implemented"
  peekWith f p = do
    ptr_a <- #{peek DummyHsPair, fst_} p
    fs <- peek ptr_a
    ptr_b <- #{peek DummyHsPair, snd_} p
    sn <- f ptr_b
    return $ HsPair (fs, sn)

-- HsString
newtype HsString = HsString
  { hsString :: String
  }

instance Addressable HsString

instance Storable HsString where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = error "HsStruct.HsString: poke not implemented"
  peek p = fmap HsString $ peekCStringLen =<< peekStrLen p

newtype HsByteString = HsByteString
  { hsByteString :: ByteString
  }

instance Addressable HsByteString

instance Storable HsByteString where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = error "HsStruct.HsByteString: poke not implemented"
  peek p = fmap HsByteString $ packCStringLen =<< peekStrLen p

newtype HsText = HsText
  { hsText :: Text
  }

instance Addressable HsText

instance Storable HsText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = error "HsStruct.HsText: poke not implemented"
  peek p = fmap HsText $ uncurry cStringLenToText =<< peekStrLen p

newtype HsLenientText = HsLenientText
  { hsLenientText :: Text
  }

instance Addressable HsLenientText

instance Storable HsLenientText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = error "HsStruct.HsLenientText: poke not implemented"
  peek p = fmap HsLenientText $ uncurry cStringLenToTextLenient =<< peekStrLen p

peekStrLen :: Ptr a -> IO CStringLen
peekStrLen p = do
  (str :: CString) <- #{peek HsString, str} p
  (len :: CSize) <- #{peek HsString, len} p
  return (str, fromIntegral len)

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
  pokeWith = error "HsStruct.HsArray: pokeWith not implemented"
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
  pokeWith = error "HsStruct.HsList: pokeWith not implemented"
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
  pokeWith = error "HsStruct.HsMap: pokeWith not implemented"
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
  pokeWith = error "HsStruct.HsIntMap: pokeWith not implemented"
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
  pokeWith = error "HsStruct.HsHashMap: pokeWith not implemented"
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
  pokeWith = error "HsStruct.HsObject: pokeWith not implemented"
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
  poke = error "HsStruct.HsJSON: poke not implemented"
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

$(deriveDestructibleUnsafe "HsMaybeInt" [t| HsMaybe Int |])
$(deriveDestructibleUnsafe "HsMaybeDouble" [t| HsMaybe Double |])
$(deriveDestructibleUnsafe "HsMaybeString" [t| HsMaybe HsByteString |])
$(deriveDestructibleUnsafe "HsMaybeString" [t| HsMaybe HsText |])

$(deriveDestructibleUnsafe "HsEitherStringInt" [t| HsEither HsText Int |])
$(deriveDestructibleUnsafe "HsEitherStringDouble" [t| HsEither HsText Double |])
$(deriveDestructibleUnsafe "HsEitherStringString" [t| HsEither HsText HsByteString |])
$(deriveDestructibleUnsafe "HsEitherStringString" [t| HsEither HsText HsText |])

$(deriveDestructibleUnsafe "HsEitherStringArrayInt" [t| HsEither HsText (HsList Int) |])
$(deriveDestructibleUnsafe "HsEitherStringArrayDouble" [t| HsEither HsText (HsList Double) |])
$(deriveDestructibleUnsafe "HsEitherStringArrayString" [t| HsEither HsText (HsList HsByteString) |])
$(deriveDestructibleUnsafe "HsEitherStringArrayString" [t| HsEither HsText (HsList HsText) |])

$(deriveDestructibleUnsafe "HsString" [t| HsString |])
$(deriveDestructibleUnsafe "HsString" [t| HsByteString |])
$(deriveDestructibleUnsafe "HsString" [t| HsText |])
$(deriveDestructibleUnsafe "HsString" [t| HsLenientText |])

$(deriveDestructibleUnsafe "HsArrayInt" [t| HsList Int |])
$(deriveDestructibleUnsafe "HsArrayDouble" [t| HsList Double |])
$(deriveDestructibleUnsafe "HsArrayString" [t| HsList HsByteString |])
$(deriveDestructibleUnsafe "HsArrayString" [t| HsList HsText |])

$(deriveDestructibleUnsafe "HsMapIntInt" [t| HsIntMap Int |])
$(deriveDestructibleUnsafe "HsMapIntDouble" [t| HsIntMap Double |])
$(deriveDestructibleUnsafe "HsMapIntString" [t| HsIntMap HsByteString |])
$(deriveDestructibleUnsafe "HsMapIntString" [t| HsIntMap HsText |])
$(deriveDestructibleUnsafe "HsMapStringInt" [t| HsObject Int |])
$(deriveDestructibleUnsafe "HsMapStringDouble" [t| HsObject Double |])
$(deriveDestructibleUnsafe "HsMapStringString" [t| HsObject HsByteString |])
$(deriveDestructibleUnsafe "HsMapStringString" [t| HsObject HsText |])

$(deriveDestructibleUnsafe "HsJSON" [t| HsJSON |])
