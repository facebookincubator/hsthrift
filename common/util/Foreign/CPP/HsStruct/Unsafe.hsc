-- Copyright (c) Facebook, Inc. and its affiliates.

module Foreign.CPP.HsStruct.Unsafe
  ( unsafeMaybeRelease
  , unsafeMaybeSteal
  , unsafeToHsStringPiece
  ) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign

import Foreign.CPP.Marshallable
import Foreign.CPP.HsStruct.Types

#include <cpp/HsStruct.h>

-- | Returns the raw pointer to the optional foreign object in the 'HsMaybe'.
-- The 'HsMaybe' releases the ownership and becomes owning nothing.
-- Think is as calling @std::unique_ptr::release()@.
--
-- Haskell takes the responsibility of handling exceptions including
-- asynchronous exceptions and ensure the foreign object is disposed of
-- properly. Do /not/ use this function unless you intend to take over the
-- ownership /and/ extend the lifetime of the foreign object. Otherwise,
-- simply leave the ownership to C++ and keep the 'HsMaybe' intact.
--
-- Use 'unsafeMaybeSteal' whenever the foreign object can be disposed of by
-- calling 'delete' from 'Destructible'.
unsafeMaybeRelease :: Ptr (HsMaybe a) -> IO (Ptr a)
unsafeMaybeRelease p = do
  ptr <- #{peek DummyHsMaybe, ptr} p
  #{poke DummyHsMaybe, ptr} p nullPtr
  return ptr

-- | Steals the raw pointer to the optional foreign object in 'HsMaybe'.
-- Haskell takes over the ownership and takes the responsibility of disposing
-- of it by calling 'delete'. The 'HsMaybe' releases the ownership and becomes
-- owning nothing. Think it as calling
-- @std::shared_ptr::shared_ptr(std::unique_ptr&&)@.
unsafeMaybeSteal :: Destructible a => Ptr (HsMaybe a) -> IO (ForeignPtr a)
unsafeMaybeSteal p = mask_ $ toSharedPtr =<< unsafeMaybeRelease p

unsafeToHsStringPiece :: ByteString -> IO HsStringPiece
unsafeToHsStringPiece s = unsafeUseAsCStringLen s $ return . uncurry HsRange
