-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.CPP.HsStruct.Types
  ( StorableContainer(..)
  -- * HsRange
  , HsRange(..)
  , HsStringPiece
  , withByteStringAsHsStringPiece
  , withTextAsHsStringPiece
  -- * HsMaybe
  , HsMaybe(..)
  -- * HsOption
  , HsOption(..)
  -- * HsEither
  , HsEither(..)
  , peekHsEitherWith
  -- * HsPair
  , HsPair(..)
  , peekHsPairWith
  -- * HsStdTuple
  , HsStdTuple(..)
  -- * HsString
  , HsString(..)
  , HsByteString(..)
  , HsText(..)
  , HsLenientText(..)
  -- * HsArray
  , HsArray(..)
  , HsArrayStorable(..)
  , HsList(..)
  -- * HsMap
  , HsMap(..)
  , HsIntMap(..)
  , HsHashMap(..)
  , HsObject(..)
  , HsHashSet(..)
  -- * HsJSON
  , HsJSON(..)
  ) where

import Control.DeepSeq (NFData)
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as KM
#endif
import Data.Aeson (Value(..))
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)
import Data.Scientific
  (fromFloatDigits, floatingOrInteger, toBoundedInteger, toRealFloat)
import Data.Vector (Vector, generateM)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as Vector
import Foreign hiding (void)
import Foreign.C
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Foreign.CPP.Addressable hiding (alignment, sizeOf)
import qualified Foreign.CPP.Addressable as Addressable
import Foreign.CPP.HsStruct.HsArray
import Foreign.CPP.HsStruct.HsSet
import Foreign.CPP.HsStruct.HsMap
import Foreign.CPP.HsStruct.HsOption
import Foreign.CPP.HsStruct.HsStdTuple
import Foreign.CPP.HsStruct.Utils
import Foreign.CPP.Marshallable.TH
import Mangle.TH
import Util.Text (cStringLenToText, cStringLenToTextLenient)

#include <hsc.h>
#include <cpp/HsOption.h>
#include <cpp/HsStdTuple.h>
#include <cpp/HsStdVariant.h>
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

-- Derive Marshallables first

$(deriveMarshallableUnsafe "HsOptionBool" [t| HsOption Bool |])
$(deriveMarshallableUnsafe "HsOptionBool" [t| HsOption CBool |])
$(deriveMarshallableUnsafe "HsOptionInt16" [t| HsOption CShort |])
$(deriveMarshallableUnsafe "HsOptionInt16" [t| HsOption Int16 |])
$(deriveMarshallableUnsafe "HsOptionInt32" [t| HsOption CInt |])
$(deriveMarshallableUnsafe "HsOptionInt32" [t| HsOption Int32 |])
$(deriveMarshallableUnsafe "HsOptionInt64" [t| HsOption CLong |])
$(deriveMarshallableUnsafe "HsOptionInt64" [t| HsOption Int64 |])
$(deriveMarshallableUnsafe "HsOptionUInt32" [t| HsOption CUInt |])
$(deriveMarshallableUnsafe "HsOptionUInt32" [t| HsOption Word32 |])
$(deriveMarshallableUnsafe "HsOptionUInt64" [t| HsOption CULong |])
$(deriveMarshallableUnsafe "HsOptionUInt64" [t| HsOption Word64 |])
$(deriveMarshallableUnsafe "HsOptionFloat" [t| HsOption Float |])
$(deriveMarshallableUnsafe "HsOptionFloat" [t| HsOption CFloat |])
$(deriveMarshallableUnsafe "HsOptionDouble" [t| HsOption Double |])
$(deriveMarshallableUnsafe "HsOptionDouble" [t| HsOption CDouble |])


-- Derive constructions after marshallable
$(#{derive_hs_option_unsafe Bool} [t| Bool |])
$(#{derive_hs_option_unsafe Bool} [t| CBool |])
$(#{derive_hs_option_unsafe Int16} [t| CShort |])
$(#{derive_hs_option_unsafe Int16} [t| Int16 |])
$(#{derive_hs_option_unsafe Int32} [t| CInt |])
$(#{derive_hs_option_unsafe Int32} [t| Int32 |])
$(#{derive_hs_option_unsafe Int64} [t| CLong |])
$(#{derive_hs_option_unsafe Int64} [t| Int64 |])
$(#{derive_hs_option_unsafe UInt32} [t| CUInt |])
$(#{derive_hs_option_unsafe UInt32} [t| Word32 |])
$(#{derive_hs_option_unsafe UInt64} [t| CULong |])
$(#{derive_hs_option_unsafe UInt64} [t| Word64 |])
$(#{derive_hs_option_unsafe Float} [t| Float |])
$(#{derive_hs_option_unsafe Float} [t| CFloat |])
$(#{derive_hs_option_unsafe Double} [t| Double |])
$(#{derive_hs_option_unsafe Double} [t| CDouble |])

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

$(mangle
  "void ctorHsStringPiece(HsRange<char>*, const char*, size_t)"
  [d|
    foreign import ccall unsafe
      c_constructHsStringPiece :: Ptr HsStringPiece -> CString -> Word -> IO ()
  |])

$(mangle
  "HsStringPiece* newHsStringPiece(const char*, size_t)"
  [d|
    foreign import ccall unsafe
      c_newHsStringPiece :: CString -> Word -> IO (Ptr HsStringPiece)
  |])

$(deriveMarshallableUnsafe "HsStringPiece" [t| HsStringPiece |])
$(deriveMarshallableUnsafe "HsOptionStringView" [t| HsOption HsStringPiece |])
$(#{derive_hs_option_unsafe StringView} [t| HsStringPiece |])

instance Constructible HsStringPiece where
  newValue (HsRange str len) =
    castPtr <$> c_newHsStringPiece str (fromIntegral len)
  constructValue p (HsRange str len) =
    c_constructHsStringPiece (castPtr p) str (fromIntegral len)

instance Addressable HsStringPiece

withByteStringAsHsStringPiece
  :: ByteString -> (HsStringPiece -> IO res) -> IO res
withByteStringAsHsStringPiece b act = unsafeUseAsCStringLen b $ \cstrlen ->
  act (uncurry HsRange cstrlen)

withTextAsHsStringPiece :: Text -> (HsStringPiece -> IO res) -> IO res
withTextAsHsStringPiece b = withByteStringAsHsStringPiece (Text.encodeUtf8 b)

-- HsMaybe --------------------------------------------------------------------

newtype HsMaybe a = HsMaybe
  { hsMaybe :: Maybe a
  }

$(deriveMarshallableUnsafe "HsMaybeInt" [t| HsMaybe Int |])
$(deriveMarshallableUnsafe "HsMaybeDouble" [t| HsMaybe Double |])

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

$(deriveMarshallableUnsafe "HsString" [t| HsString |])

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

instance Constructible HsText where
  newValue (HsText txt) = newStrImpl (Text.encodeUtf8 txt)
  constructValue p (HsText txt) =
    constructStrImpl (castPtr p) (Text.encodeUtf8 txt)

instance Addressable HsText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}

instance Storable HsText where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = error "HsStruct.HsText: poke not implemented"
  peek p = fmap HsText $ uncurry cStringLenToText =<< peekStrLen p

$(deriveMarshallableUnsafe "HsString" [t| HsText |])
$(deriveMarshallableUnsafe "HsOptionString" [t| HsOption HsText |])
$(#{derive_hs_option_unsafe String} [t| HsText |])
$(deriveMarshallableUnsafe "HsMaybeString" [t| HsMaybe HsText |])

instance Assignable HsText

instance Eq HsText where
  (HsText a) == (HsText b) = a == b

instance Hashable HsText where
  hashWithSalt i (HsText a) = hashWithSalt i a

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

$(deriveMarshallableUnsafe "HsEitherStringInt" [t| HsEither HsText Int |])
$(deriveMarshallableUnsafe "HsEitherStringDouble" [t| HsEither HsText Double |])
$(deriveMarshallableUnsafe "HsEitherStringString" [t| HsEither HsText HsText |])

instance Addressable1 (HsEither a) where
  sizeOf1 _ = #{size DummyHsEither}
  alignment1 _ = #{alignment DummyHsEither}

instance Addressable (HsEither a b) where
  sizeOf = sizeOf1
  alignment = alignment1

instance Storable a => StorableContainer (HsEither a) where
  pokeWith = notPokeable "HsEither"
  peekWith = peekHsEitherWith peek

peekHsEitherWith
  :: (Ptr a -> IO x)
  -> (Ptr b -> IO y)
  -> Ptr (HsEither a b)
  -> IO (HsEither x y)
peekHsEitherWith peekLeft peekRight p = do
  isLeft <- #{peek DummyHsEither, isLeft} p :: IO CChar
  if toBool isLeft
  then do
    ptr <- #{peek DummyHsEither, left} p
    HsEither . Left <$> peekLeft ptr
  else do
    ptr <- #{peek DummyHsEither, right} p
    HsEither . Right <$> peekRight ptr

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

instance Storable a => StorableContainer (HsPair a) where
  pokeWith = notPokeable "HsPair"
  peekWith = peekHsPairWith peek

peekHsPairWith
  :: (Ptr a -> IO x)
  -> (Ptr b -> IO y)
  -> Ptr (HsPair a b)
  -> IO (HsPair x y)
peekHsPairWith peekFirst peekSecond p = do
  ptr_a <- #{peek DummyHsPair, fst_} p
  fs <- peekFirst ptr_a
  ptr_b <- #{peek DummyHsPair, snd_} p
  sn <- peekSecond ptr_b
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

instance Storable HsByteString where
  sizeOf _ = #{size HsString}
  alignment _ = #{alignment HsString}
  poke = notPokeable "HsByteString"
  peek p = fmap HsByteString $ packCStringLen =<< peekStrLen p

$(deriveMarshallableUnsafe "HsString" [t| HsByteString |])
$(deriveMarshallableUnsafe "HsOptionString" [t| HsOption HsByteString |])
$(#{derive_hs_option_unsafe String} [t| HsByteString |])
$(deriveMarshallableUnsafe "HsMaybeString" [t| HsMaybe HsByteString |])
$(deriveMarshallableUnsafe "HsEitherStringString" [t| HsEither HsText HsByteString |])

instance Assignable HsByteString
instance Assignable (HsEither HsText HsByteString)


-- HsLenientText --------------------------------------------------------------

newtype HsLenientText = HsLenientText
  { hsLenientText :: Text
  }

$(deriveMarshallableUnsafe "HsString" [t| HsLenientText |])

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

peekDataSize :: Ptr b -> IO (Ptr a, Int)
peekDataSize p = do
  (a :: Ptr a) <- #{peek DummyHsArray, a} p
  (n :: CSize) <- #{peek DummyHsArray, n} p
  return (a, fromIntegral n)

$(deriveMarshallableUnsafe "HsArrayInt16" [t| HsArray CShort |])
$(deriveMarshallableUnsafe "HsArrayInt16" [t| HsArray Int16 |])
$(deriveMarshallableUnsafe "HsArrayInt32" [t| HsArray CInt |])
$(deriveMarshallableUnsafe "HsArrayInt32" [t| HsArray Int32 |])
$(deriveMarshallableUnsafe "HsArrayInt64" [t| HsArray Int |])
$(deriveMarshallableUnsafe "HsArrayInt64" [t| HsArray Int64 |])
$(deriveMarshallableUnsafe "HsArrayInt64" [t| HsArray CLong |])
$(deriveMarshallableUnsafe "HsArrayUInt8" [t| HsArray CBool |])
$(deriveMarshallableUnsafe "HsArrayUInt8" [t| HsArray Word8 |])
$(deriveMarshallableUnsafe "HsArrayUInt32" [t| HsArray CUInt |])
$(deriveMarshallableUnsafe "HsArrayUInt32" [t| HsArray Word32 |])
$(deriveMarshallableUnsafe "HsArrayUInt64" [t| HsArray CULong |])
$(deriveMarshallableUnsafe "HsArrayUInt64" [t| HsArray Word64 |])
$(deriveMarshallableUnsafe "HsArrayFloat" [t| HsArray Float |])
$(deriveMarshallableUnsafe "HsArrayFloat" [t| HsArray CFloat |])
$(deriveMarshallableUnsafe "HsArrayDouble" [t| HsArray Double |])
$(deriveMarshallableUnsafe "HsArrayDouble" [t| HsArray CDouble |])
$(deriveMarshallableUnsafe "HsArrayString" [t| HsArray HsByteString |])
$(deriveMarshallableUnsafe "HsArrayString" [t| HsArray HsText |])

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

-- | 'HsArrayStorable' is 'HsArray' with a more efficient
-- representation, which can be passed back out to C++ without copying
-- using Vector.Storable.unsafeWith.
newtype HsArrayStorable a = HsArrayStorable
  { hsArrayStorable :: VS.Vector a
  }

instance Storable a => Storable (HsArrayStorable a) where
  sizeOf _ = #{size DummyHsArray}
  alignment _ = #{alignment DummyHsArray}
  poke = notPokeable "HsArrayStorable"
  peek p = do
    (a, n) <- peekDataSize p
    arr <- mallocForeignPtrArray n
    withForeignPtr arr $ \parr -> copyBytes parr a (n * sizeOf (undefined :: a))
    return (HsArrayStorable (VS.unsafeFromForeignPtr0 arr n))

-- HsList
newtype HsList a = HsList
  { hsList :: [a]
  }

$(deriveMarshallableUnsafe "HsArrayInt16" [t| HsList CShort |])
$(deriveMarshallableUnsafe "HsArrayInt16" [t| HsList Int16 |])
$(deriveMarshallableUnsafe "HsArrayInt32" [t| HsList CInt |])
$(deriveMarshallableUnsafe "HsArrayInt32" [t| HsList Int32 |])
$(deriveMarshallableUnsafe "HsArrayInt64" [t| HsList Int |])
$(deriveMarshallableUnsafe "HsArrayInt64" [t| HsList Int64 |])
$(deriveMarshallableUnsafe "HsArrayInt64" [t| HsList CLong |])
$(deriveMarshallableUnsafe "HsArrayUInt8" [t| HsList CBool |])
$(deriveMarshallableUnsafe "HsArrayUInt8" [t| HsList Word8 |])
$(deriveMarshallableUnsafe "HsArrayUInt32" [t| HsList CUInt |])
$(deriveMarshallableUnsafe "HsArrayUInt32" [t| HsList Word32 |])
$(deriveMarshallableUnsafe "HsArrayUInt64" [t| HsList CULong |])
$(deriveMarshallableUnsafe "HsArrayUInt64" [t| HsList Word64 |])
$(deriveMarshallableUnsafe "HsArrayFloat" [t| HsList Float |])
$(deriveMarshallableUnsafe "HsArrayFloat" [t| HsList CFloat |])
$(deriveMarshallableUnsafe "HsArrayDouble" [t| HsList Double |])
$(deriveMarshallableUnsafe "HsArrayDouble" [t| HsList CDouble |])
$(deriveMarshallableUnsafe "HsArrayString" [t| HsList HsByteString |])
$(deriveMarshallableUnsafe "HsArrayString" [t| HsList HsText |])
$(deriveHsArrayUnsafe "Int16" [t| CShort |])
$(deriveHsArrayUnsafe "Int16" [t| Int16 |])
$(deriveHsArrayUnsafe "Int32" [t| CInt |])
$(deriveHsArrayUnsafe "Int32" [t| Int32 |])
$(deriveHsArrayUnsafe "Int64" [t| CLong |])
$(deriveHsArrayUnsafe "Int64" [t| Int64 |])
$(deriveHsArrayUnsafe "UInt8" [t| CBool |])
$(deriveHsArrayUnsafe "UInt8" [t| Word8 |])
$(deriveHsArrayUnsafe "UInt32" [t| CUInt |])
$(deriveHsArrayUnsafe "UInt32" [t| Word32 |])
$(deriveHsArrayUnsafe "UInt64" [t| CULong |])
$(deriveHsArrayUnsafe "UInt64" [t| Word64 |])
$(deriveHsArrayUnsafe "Float" [t| Float |])
$(deriveHsArrayUnsafe "Float" [t| CFloat |])
$(deriveHsArrayUnsafe "Double" [t| Double |])
$(deriveHsArrayUnsafe "Double" [t| CDouble |])
$(deriveHsArrayUnsafe "StringView" [t| HsStringPiece |])
$(deriveHsArrayUnsafe "String" [t| HsText |])
$(deriveHsArrayUnsafe "String" [t| HsByteString |])
$(deriveMarshallableUnsafe "HsEitherStringArrayInt" [t| HsEither HsText (HsList Int) |])
$(deriveMarshallableUnsafe "HsEitherStringArrayDouble" [t| HsEither HsText (HsList Double) |])
$(deriveMarshallableUnsafe "HsEitherStringArrayString" [t| HsEither HsText (HsList HsByteString) |])
$(deriveMarshallableUnsafe "HsEitherStringArrayString" [t| HsEither HsText (HsList HsText) |])

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

-- HsSet
newtype HsHashSet a = HsHashSet
  -- Contains a List as a transport for the internal datatype that may not be
  -- immediately hashable in haskell (i.e. CLong is not hashable, but the end
  -- type of Int is)
  { hsHashSet :: HashSet.HashSet a
  }

$(deriveMarshallableUnsafe "HsSetInt32" [t| HsHashSet CInt |])
$(deriveMarshallableUnsafe "HsSetInt32" [t| HsHashSet Int32 |])
$(deriveMarshallableUnsafe "HsSetInt64" [t| HsHashSet Int |])
$(deriveMarshallableUnsafe "HsSetInt64" [t| HsHashSet Int64 |])
$(deriveMarshallableUnsafe "HsSetInt64" [t| HsHashSet CLong |])
$(deriveMarshallableUnsafe "HsSetUInt8" [t| HsHashSet CBool |])
$(deriveMarshallableUnsafe "HsSetUInt8" [t| HsHashSet Word8 |])
$(deriveMarshallableUnsafe "HsSetUInt32" [t| HsHashSet CUInt |])
$(deriveMarshallableUnsafe "HsSetUInt32" [t| HsHashSet Word32 |])
$(deriveMarshallableUnsafe "HsSetUInt64" [t| HsHashSet CULong |])
$(deriveMarshallableUnsafe "HsSetUInt64" [t| HsHashSet Word64 |])
$(deriveMarshallableUnsafe "HsSetFloat" [t| HsHashSet Float |])
$(deriveMarshallableUnsafe "HsSetDouble" [t| HsHashSet Double |])
$(deriveMarshallableUnsafe "HsSetString" [t| HsHashSet HsByteString |])
$(deriveMarshallableUnsafe "HsSetString" [t| HsHashSet HsText |])
$(deriveHsHashSetUnsafe "Int16" [t| CShort |])
$(deriveHsHashSetUnsafe "Int16" [t| Int16 |])
$(deriveHsHashSetUnsafe "Int32" [t| CInt |])
$(deriveHsHashSetUnsafe "Int32" [t| Int32 |])
$(deriveHsHashSetUnsafe "Int64" [t| Int |])
$(deriveHsHashSetUnsafe "Int64" [t| Int64 |])
$(deriveHsHashSetUnsafe "Int64" [t| CLong |])
$(deriveHsHashSetUnsafe "UInt8" [t| CBool |])
$(deriveHsHashSetUnsafe "UInt8" [t| Word8 |])
$(deriveHsHashSetUnsafe "UInt32" [t| CUInt |])
$(deriveHsHashSetUnsafe "UInt32" [t| Word32 |])
$(deriveHsHashSetUnsafe "UInt64" [t| CULong |])
$(deriveHsHashSetUnsafe "UInt64" [t| Word64 |])
$(deriveHsHashSetUnsafe "Float" [t| Float |])
$(deriveHsHashSetUnsafe "Double" [t| Double |])
$(deriveHsHashSetUnsafe "String" [t| HsByteString |])
$(deriveHsHashSetUnsafe "String" [t| HsText |])

instance Addressable1 HsHashSet where
  sizeOf1 = setSizeOf
  alignment1 = setAlignment

instance Addressable (HsHashSet a) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable a, Eq a, Hashable a, Storable a)
  => Storable (HsHashSet a) where
  sizeOf = sizeOf1
  alignment = alignment1
  poke = notPokeable "HsHashSet"
  peek p = HsHashSet <$> peekSetWith HashSet.empty HashSet.insert peek p

{-# INLINE peekSetWith #-}
peekSetWith
  :: (Addressable a)
  => s
  -> (k -> s -> s)
  -> (Ptr a -> IO k)
  -> Ptr b
  -> IO s
peekSetWith empty insert f p = do
  (a :: Ptr a) <- #{peek DummyHsSet, keys} p
  (n :: Int) <- (fromIntegral :: CSize -> Int) <$> #{peek DummyHsSet, n} p
  let go !i !hs
        | i >= n = return hs
        | otherwise = do
            k <- peekElemOffWith f a i
            go (i + 1) $ insert k hs
  go 0 empty

setSizeOf, setAlignment :: a -> Int
setSizeOf _ = #{size DummyHsSet}
setAlignment _ = #{alignment DummyHsSet}

-- HsMap
newtype HsMap k v = HsMap
  { hsMap :: Map k v
  }

mapSizeOf, mapAlignment :: a -> Int
mapSizeOf _ = #{size DummyHsObject}
mapAlignment _ = #{alignment DummyHsObject}

instance Addressable1 (HsMap k) where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsMap k v) where
  sizeOf = sizeOf1
  alignment = alignment1

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

instance (Addressable k, Ord k, Storable k) => StorableContainer (HsMap k) where
  pokeWith = notPokeable "HsMap"
  peekWith f p = HsMap <$> peekMapWith Map.empty Map.insert peek f p

newtype HsIntMap v = HsIntMap
  { hsIntMap :: IntMap v
  }

$(deriveMarshallableUnsafe "HsMapIntInt" [t| HsIntMap Int |])
$(deriveMarshallableUnsafe "HsMapIntDouble" [t| HsIntMap Double |])
$(deriveMarshallableUnsafe "HsMapIntString" [t| HsIntMap HsByteString |])
$(deriveMarshallableUnsafe "HsMapIntString" [t| HsIntMap HsText |])

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

$(deriveMarshallableUnsafe "HsMapIntInt" [t| HsHashMap Int Int |])
$(deriveMarshallableUnsafe "HsMapInt32String" [t| HsHashMap Int32 HsText |])
$(deriveMarshallableUnsafe "HsMapDoubleInt32" [t| HsHashMap Double Int32 |])
$(deriveMarshallableUnsafe "HsMapStringString" [t| HsHashMap HsText HsText |])
-- O(n^2) derivations, try to derive just what you need
$(deriveHsHashMapUnsafe "IntInt" [t| Int |] [t| Int |])
$(deriveHsHashMapUnsafe "Int32Int32" [t| Int32 |] [t| Int32 |])
$(deriveHsHashMapUnsafe "Int32Double" [t| Int32 |] [t| Double |])
$(deriveHsHashMapUnsafe "Int32String" [t| Int32 |] [t| HsByteString |])
$(deriveHsHashMapUnsafe "Int32String" [t| Int32 |] [t| HsText |])
$(deriveHsHashMapUnsafe "DoubleInt32" [t| Double |] [t| Int32 |])
$(deriveHsHashMapUnsafe "StringInt32" [t| HsByteString |] [t| Int32 |])
$(deriveHsHashMapUnsafe "StringInt32" [t| HsText |] [t| Int32 |])
$(deriveHsHashMapUnsafe "StringDouble" [t| HsText |] [t| Double |])
$(deriveHsHashMapUnsafe "StringDouble" [t| HsByteString |] [t| Double |])
$(deriveHsHashMapUnsafe "StringString" [t| HsText |] [t| HsText |])
$(deriveHsHashMapUnsafe "StringString" [t| HsText |] [t| HsByteString |])
$(deriveHsHashMapUnsafe "StringString" [t| HsByteString |] [t| HsText |])
$(deriveHsHashMapUnsafe "StringString" [t| HsByteString |] [t| HsByteString |])

instance Addressable1 (HsHashMap k) where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsHashMap k v) where
  sizeOf = sizeOf1
  alignment = alignment1

instance (Addressable k, Eq k, Hashable k, Storable k)
  => StorableContainer (HsHashMap k) where
  pokeWith = notPokeable "HsHashMap"
  peekWith f p = HsHashMap <$> peekMapWith HashMap.empty HashMap.insert peek f p

newtype HsObject v = HsObject
#if MIN_VERSION_aeson(2,0,0)
  { hsObject :: KeyMap v
#else
  { hsObject :: HashMap Text v
#endif
  }

$(deriveMarshallableUnsafe "HsMapStringInt" [t| HsObject Int |])
$(deriveMarshallableUnsafe "HsMapStringDouble" [t| HsObject Double |])
$(deriveMarshallableUnsafe "HsMapStringString" [t| HsObject HsByteString |])
$(deriveMarshallableUnsafe "HsMapStringString" [t| HsObject HsText |])

instance Addressable1 HsObject where
  sizeOf1 = mapSizeOf
  alignment1 = mapAlignment

instance Addressable (HsObject v) where
  sizeOf = sizeOf1
  alignment = alignment1

instance StorableContainer HsObject where
  pokeWith = notPokeable "HsObject"
  peekWith f p = HsObject <$> peekMapWith emptyMap insertMap fk f p
    where
#if MIN_VERSION_aeson(2,0,0)
    fk = fmap (KM.fromText . hsText) . peek
#else
    fk = fmap hsText . peek
#endif
    emptyMap = mempty
    insertMap =
#if MIN_VERSION_aeson(2,0,0)
      KM.insert
#else
      HashMap.insert
#endif

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

$(mangle
  "void ctorHsJSONNull(HsJSON*)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONNull :: Ptr HsJSON -> IO ()
  |])

$(mangle
  "void ctorHsJSONBool(HsJSON*, bool)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONBool :: Ptr HsJSON -> CBool -> IO ()
  |])

$(mangle
  "void ctorHsJSONInt(HsJSON*, int64_t)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONInt :: Ptr HsJSON -> CLong -> IO ()
  |])

$(mangle
  "void ctorHsJSONDouble(HsJSON*, double)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONDouble :: Ptr HsJSON -> CDouble -> IO ()
  |])

$(mangle
  "void ctorHsJSONString(HsJSON*, HsString*)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONString :: Ptr HsJSON -> Ptr HsText -> IO ()
  |])

$(mangle
  "void ctorHsJSONArray(HsJSON*, HsArray<HsJSON>*)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONArray :: Ptr HsJSON -> Ptr (HsArray HsJSON) -> IO ()
  |])

$(mangle
  "void ctorHsJSONObject(HsJSON*, HsMap<HsString, HsJSON>*)"
  [d|
    foreign import ccall unsafe
      c_constructHsJSONObject :: Ptr HsJSON -> Ptr (HsObject HsJSON) -> IO ()
  |])

$(deriveMarshallableUnsafe "HsArrayJSON" [t| HsArray HsJSON |])
$(deriveMarshallableUnsafe "HsOptionHsJSON" [t| HsOption HsJSON |])
$(deriveMarshallableUnsafe "HsArrayJSON" [t| HsList HsJSON |])
$(deriveMarshallableUnsafe "HsSetJSON" [t| HsHashSet HsJSON |])
$(deriveMarshallableUnsafe "HsObjectJSON" [t| HsObject HsJSON |])
$(deriveHsArrayUnsafe "HsJSON" [t| HsJSON |])

foreign import ccall unsafe "common_hs_ctorHsObjectJSON"
  c_constructHsObjectJSON
    :: Ptr (HsObject HsJSON)
    -> Ptr (HsArray HsText)
    -> Ptr (HsArray HsJSON)
    -> IO ()

instance Constructible (HsObject HsJSON) where
  newValue (HsObject _o) = error "HsObject HsJSON cannot be made on heap"
  constructValue ptr (HsObject m) =
    withCxxObject (HsArray (Vector.fromList (map HsText keys))) $ \keys_p ->
    withCxxObject (HsArray (Vector.fromList vals)) $ \vals_p ->
      c_constructHsObjectJSON ptr keys_p vals_p
    where
      (keys, vals) = unzip $
#if MIN_VERSION_aeson(2,0,0)
        map (\(k, v) -> (KM.toText k, v)) $ KM.toList m
#else
        HashMap.toList m
#endif

instance Constructible HsJSON where
  newValue (HsJSON _val) = error $ "HsStruct.HsJSON cannot be built on heap"
  constructValue ptr (HsJSON val) =
    case val of
      Null -> c_constructHsJSONNull ptr
      Bool b -> c_constructHsJSONBool ptr (fromBool b)
      Number n -> case (floatingOrInteger n :: Either CDouble CLong) of
        Left r -> c_constructHsJSONDouble ptr r
        Right _ -> case toBoundedInteger n of
          Just i -> c_constructHsJSONInt ptr i
          Nothing -> c_constructHsJSONDouble ptr $ toRealFloat n
      String txt -> withCxxObject (HsText txt) $ c_constructHsJSONString ptr
      Array v -> withCxxObject (HsArray (Vector.map HsJSON v)) $
        c_constructHsJSONArray ptr
      Object o -> withCxxObject (HsObject (HsJSON <$> o)) $
        c_constructHsJSONObject ptr

$(deriveMarshallableUnsafe "HsJSON" [t| HsJSON |])
$(#{derive_hs_option_unsafe HsJSON} [t| HsJSON |])
$(deriveHsHashSetUnsafe "HsJSON" [t| HsJSON |])
