-- Copyright (c) Facebook, Inc. and its affiliates.

--
-- Marshal folly::dynamic directly into Haskell's Data.Aeson,
-- and in the other direction.
--

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-# OPTIONS -fno-warn-unused-imports #-} -- broken on this module
module Foreign.CPP.Dynamic
  ( Dynamic
  , readDynamic
  , readDynamicLenient
  , createDynamic
  , destroyDynamic
  , withDynamic
  , parseJSON
  ) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
import qualified Data.ByteString as B
import Foreign.C
import Foreign hiding (alloca, allocaBytes, allocaArray)
-- Custom alloca and friends
import Util.Memory
import Util.ByteString

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.Aeson hiding (parseJSON)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Scientific

import Mangle.TH
import Foreign.CPP.Marshallable.TH
import Util.Text
  ( cStringToText
  , cStringToTextLenient
  , useTextAsCString
  , useTextsAsCStrings )
import Util.String.Quasi

-- | Phantom type for @folly::dynamic@ pointers.
newtype Dynamic = Dynamic
  { unDynamic :: Value }

$(deriveDestructibleUnsafe "Dynamic" [t| Dynamic |])

newtype DType = DType #{type DType}
  deriving (Eq, Storable)

#{enum DType, DType
  , tNull   = tNull
  , tArray  = tArray
  , tBool   = tBool
  , tDouble = tDouble
  , tInt64  = tInt64
  , tObject = tObject
  , tString = tString
  }

$(mangle
  [s|
    void facebook::hs::readDynamic(
        const folly::dynamic*, DType*, DValue*)
  |] [d|
    foreign import ccall unsafe
      c_readDynamic :: Ptr Dynamic -> Ptr DType -> Ptr () -> IO ()
  |])

$(mangle
  [s|
    int facebook::hs::readDynamicArray(
      const folly::dynamic*, size_t, const folly::dynamic**)
  |] [d|
    foreign import ccall unsafe
      c_readDynamicArray :: Ptr Dynamic -> CSize
                         -> Ptr (Ptr Dynamic)
                         -> IO CInt
  |])

$(mangle
  [s|
    int facebook::hs::readDynamicObject(
       const folly::dynamic*, size_t,
       const folly::dynamic**, const folly::dynamic**)
  |] [d|
    foreign import ccall unsafe
      c_readDynamicObject :: Ptr Dynamic -> CSize
                          -> Ptr (Ptr Dynamic)
                          -> Ptr (Ptr Dynamic)
                          -> IO CInt
  |])

$(mangle
  "void facebook::hs::createDynamic(folly::dynamic*, DType, DValue*)"
  [d|
    foreign import ccall unsafe
      c_createDynamic :: Ptr Dynamic -> DType -> Ptr () -> IO ()
  |])

$(mangle
  [s|
    void facebook::hs::createDynamicArray(
      folly::dynamic*, size_t, folly::dynamic*)
  |] [d|
    foreign import ccall unsafe
      c_createDynamicArray :: Ptr Dynamic -> CSize -> Ptr Dynamic -> IO ()
  |])

$(mangle
  [s|
    void facebook::hs::createDynamicObject(
      folly::dynamic*, size_t, const char**, folly::dynamic*)
  |] [d|
    foreign import ccall unsafe
      c_createDynamicObject :: Ptr Dynamic
                            -> CSize
                            -> Ptr CString
                            -> Ptr Dynamic
                            -> IO ()
  |])

$(mangle
  "folly::dynamic* facebook::hs::parseJSON(const char*, int64_t, char **)"
  [d|
    foreign import ccall unsafe
      c_parseJSON :: CString -> CLong -> Ptr (Ptr CChar) -> IO (Ptr Dynamic)
  |])


$(mangle
  "folly::dynamic* facebook::hs::parseJSON(const char*, int64_t, char **)"
  [d|
    foreign import ccall safe
      c_parseJSON_safe :: CString -> CLong -> Ptr (Ptr CChar)
        -> IO (Ptr Dynamic)
  |])

-- | Reads a 'Dynamic' into an Aeson 'Value'.
--
-- Dynamic objects can have any type as the key, whereas JSON has only
-- string keys. This shows up when a PHP array has been converted to a
-- @folly::dynamic@, which will be an object with integer keys.
-- Therefore here we convert integer keys to strings to make it valid
-- JSON.
readDynamic :: Ptr Dynamic -> IO Value
readDynamic p = unDynamic <$> peek p

-- | Reads a 'Dynamic' into an Aeson 'Value' using lenient UTF-8 decoding.
readDynamicLenient :: Ptr Dynamic -> IO Value
readDynamicLenient p = unDynamic <$> peekImpl cStringToTextLenient p

-- | Creates a 'Dynamic' from an Aeson 'Value'.
--
-- Remember to call 'destroyDynamic' to free the memory.
createDynamic :: Value -> IO (Ptr Dynamic)
createDynamic v = new $ Dynamic v

-- | Frees the memory owned by 'Dynamic'
destroyDynamic :: Ptr Dynamic -> IO ()
destroyDynamic p = destruct p >> free p

-- | Executes an 'IO' action with an Aeson 'Value' marshalled as a 'Dynamic'
withDynamic :: Value -> (Ptr Dynamic -> IO a) -> IO a
withDynamic v = bracket (createDynamic v) destroyDynamic

-- | Parse JSON using folly::parseJson(), which is typically about 2x
-- faster than Aeson's family of JSON parsing functions.
parseJSON :: ByteString -> IO (Either Text Value)
parseJSON bs =
  unsafeUseAsCStringLen bs $ \(cstr, clen) ->
  Foreign.with nullPtr $ \perr -> do
    let
      cleanup pdynamic = do
        str <- peek perr
        when (str /= nullPtr) $ free str
        delete pdynamic
      ffi
        -- conservative: 100K parses in about 0.2ms
        | B.length bs > 10*1024 = c_parseJSON_safe
        | otherwise = c_parseJSON
    bracket
      (ffi cstr (fromIntegral clen) perr)
      cleanup $ \pdynamic -> do
        if pdynamic == nullPtr
          then fmap Left $ cStringToText =<< peek perr
          else Right <$> readDynamic pdynamic

#include <cpp/cdynamic.h>

peekImpl :: (CString -> IO Text) -> Ptr Dynamic -> IO Dynamic
peekImpl peekCString p = do
  alloca $ \pty -> allocaBytes #{size DValue} $ \pval ->
    let getDyn pdyn = do
          c_readDynamic pdyn pty pval
          ty <- peek pty
          if
            | ty == tNull   -> return Null
            | ty == tArray  -> getDynArray pdyn
            | ty == tBool   -> do b <- peek (castPtr pval :: Ptr CInt)
                                  return (Bool (b /= 0))
            | ty == tDouble -> do d <- peek (castPtr pval :: Ptr Double)
                                  return (Number (fromFloatDigits d))
            | ty == tInt64  -> do i <- peek (castPtr pval :: Ptr Int64)
                                  return (Number (fromIntegral i))
            | ty == tString -> do s <- peek (castPtr pval :: Ptr CString)
                                  txt <- peekCString s
                                  return (String txt)
            | ty == tObject -> getDynObject pdyn
            | otherwise -> error "Foreign.CPP.Dynamic: illegal key type"

        getDynKey pdyn = do
          c_readDynamic pdyn pty pval
          ty <- peek pty
          if
            | ty == tDouble -> do d <- peek (castPtr pval :: Ptr Double)
                                  return $! Text.pack (show d)
            | ty == tInt64  -> do i <- peek (castPtr pval :: Ptr Int64)
                                  return $! Text.pack (show i)
            | ty == tString -> do s <- peek (castPtr pval :: Ptr CString)
                                  txt <- peekCString s
                                  return txt
            | otherwise -> error "Foreign.CPP.Dynamic: illegal key type"

        getDynArray pdyn = do
          size <- peek (castPtr pval :: Ptr CSize)
          allocaArray (fromIntegral size) $ \pelems -> do
            -- could be much more efficient here
            num <- c_readDynamicArray pdyn size pelems
            elems <- peekArray (fromIntegral num) pelems
            dyns <- mapM getDyn elems
            return (Array (Vector.fromList dyns))

        getDynObject pdyn = do
          size <- peek (castPtr pval :: Ptr CSize)
          allocaArray (fromIntegral size) $ \pkeys -> do
          allocaArray (fromIntegral size) $ \pvals -> do
            num <- c_readDynamicObject pdyn size pkeys pvals
            let
                go !i !obj
                  | i >= fromIntegral num = return (Object obj)
                  | otherwise = do
                    key <- peekElemOff pkeys i >>= getDynKey
                    val <- peekElemOff pvals i >>= getDyn
                    go (i+1) (HashMap.insert key val obj)

            go 0 HashMap.empty
    in
    Dynamic <$> getDyn p

instance Storable Dynamic where
  sizeOf _ = #{size folly::dynamic}
  alignment _ = #{alignment folly::dynamic}

  peek = peekImpl cStringToText

  poke p v = do
    allocaBytes #{size DValue} $ \pval ->
      let putDyn' :: Storable a => Ptr Dynamic -> DType -> a -> IO ()
          putDyn' pdyn ty val = do
            poke (castPtr pval) val
            c_createDynamic pdyn ty pval

          putDyn pdyn Null = c_createDynamic pdyn tNull nullPtr

          putDyn pdyn (Bool b) = putDyn' pdyn tBool $ if b then 1 else 0 :: CInt

          putDyn pdyn (Number n) =
            case floatingOrInteger n of
              Left d -> putDyn' pdyn tDouble (d :: Double)
              Right i -> putDyn' pdyn tInt64 (i :: Int64)

          putDyn pdyn (String s) = useTextAsCString s $ putDyn' pdyn tString

          putDyn pdyn (Array arr) = do
            let size = Vector.length arr
            withArray' size (map Dynamic $ Vector.toList arr) $ \pelems ->
              c_createDynamicArray pdyn (fromIntegral $ size) pelems

          putDyn pdyn (Object obj) = do
            let size = HashMap.size obj
                (keys, vals) = unzip $ HashMap.toList obj
            useTextsAsCStrings keys $ \pkeys ->
              withArray' size (map Dynamic vals) $ \pvals ->
                c_createDynamicObject pdyn (fromIntegral $ size) pkeys pvals
      in
      putDyn p $ unDynamic v
    where
    withArray' n a f =
      allocaArray n $ \pa -> do
        pokeArray pa a
        f pa
