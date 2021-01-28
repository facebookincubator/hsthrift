-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE DefaultSignatures #-}
module Foreign.CPP.Marshallable
  ( withCxxObject
  , makeShared
  , toSharedPtr
  , destructAndFree
  , DefaultConstructible(..)
  , Constructible(..)
  , Destructible(..)
  , Assignable(..)
  ) where

-- Importing types for default instantiations of Constructible
import Data.Ratio (Ratio)
import Foreign.C.Types
import GHC.Fingerprint (Fingerprint)
import System.Posix.Types

import Foreign.Storable
import Foreign.Marshal.Utils
import Control.Exception (bracket, mask_)
import Foreign

-- Utility functions ----------------------------------------------------------

withCxxObject
  :: (Constructible a, Destructible a)
  => a
  -> (Ptr a -> IO b)
  -> IO b
withCxxObject val func = bracket (newValue val) delete func

-- | Returns a 'ForeignPtr' that owns and manages a foreign object through
-- a pointer and disposes of that object when the lifetime of the 'ForeignPtr'
-- ends.
-- It is similar to @std::make_shared<T>@ in C++.
-- The object pointed to is both allocated+ctor'd and
-- destructed+deallocated in C++
makeShared :: (DefaultConstructible a, Destructible a) => IO (ForeignPtr a)
makeShared = newDefault >>= toSharedPtr

-- | Returns a 'ForeignPtr' that owns and manages a foreign object through
-- a pointer and disposes of that object when the lifetime of the 'ForeignPtr'
-- ends.
toSharedPtr :: Destructible a => Ptr a -> IO (ForeignPtr a)
toSharedPtr = newForeignPtr deleteFunPtr

-- | Simply calls 'destruct' and 'free'. This should be used instead of
-- 'delete' when the memory is allocated by 'malloc' or 'mallocBytes' in
-- Haskell and the object is constructed by __placement new__ in C++, or
-- 'construct' / 'constructDefault' in Haskell.
destructAndFree :: Destructible a => Ptr a -> IO ()
destructAndFree p = destruct p >> free p

-- Typeclasses ----------------------------------------------------------------

-- | Typeclass to call into C++ allocators and constructors for objects.
class DefaultConstructible a where
  -- | Allocate the space and invoke the constructor in C++.
  --
  -- This is equivalent to __new expression__ in C++, and assumes that
  -- the object has an empty constructor defined in its class definition.
  newDefault :: IO (Ptr a)

  -- | Invoke the construct of the object in C++. It does no checks on
  -- whether the ptr points to an already constructed object.
  --
  -- This is often used with 'Foreign.Marshal.Alloc.alloca' in Haskell,
  -- to do __placement new__ in C++.
  constructDefault :: Ptr a -> IO ()


-- | Similar to @DefaultConstructible@ but also initializes the value for
-- the foreign object
-- Default implementations of storable types allow using common types like
-- Int, Char etc inside other complex types like HsEither.
-- You *must* not use these default implementations when defining instances for
-- complex C++ types. Doing so may lead to memory leaks.
class Constructible a where
  newValue :: a -> IO (Ptr a)
  default newValue :: Storable a => a -> IO (Ptr a)
  newValue = new

  constructValue :: Ptr a -> a -> IO ()
  default constructValue :: Storable a => Ptr a -> a -> IO ()
  constructValue = poke

instance Constructible Bool
instance Constructible Char
instance Constructible Double
instance Constructible Float
instance Constructible Int
instance Constructible Int8
instance Constructible Int16
instance Constructible Int32
instance Constructible Int64
instance Constructible Word
instance Constructible Word8
instance Constructible Word16
instance Constructible Word32
instance Constructible Word64
instance Constructible ()
instance Constructible Fingerprint
instance Constructible CUIntMax
instance Constructible CIntMax
instance Constructible CUIntPtr
instance Constructible CIntPtr
instance Constructible CSUSeconds
instance Constructible CUSeconds
instance Constructible CTime
instance Constructible CClock
instance Constructible CSigAtomic
instance Constructible CWchar
instance Constructible CSize
instance Constructible CPtrdiff
instance Constructible CDouble
instance Constructible CFloat
instance Constructible CULLong
instance Constructible CLLong
instance Constructible CULong
instance Constructible CLong
instance Constructible CUInt
instance Constructible CInt
instance Constructible CUShort
instance Constructible CShort
instance Constructible CUChar
instance Constructible CSChar
instance Constructible CChar
instance Constructible IntPtr
instance Constructible WordPtr
instance Constructible Fd
instance Constructible CRLim
instance Constructible CTcflag
instance Constructible CSpeed
instance Constructible CCc
instance Constructible CUid
instance Constructible CNlink
instance Constructible CGid
instance Constructible CSsize
instance Constructible CPid
instance Constructible COff
instance Constructible CMode
instance Constructible CIno
instance Constructible CDev
instance (Storable a, Integral a) => Constructible (Ratio a)


-- | Typeclass to call into C++ destructors and deallocators for objects.
class Destructible a where
  -- | Invoke the destructor to free the resources owned by the object,
  -- the storage of the object itself is untouched, the object must not be
  -- used after this.
  --
  -- This is often used with 'Foreign.Marshal.Alloc.alloca' in Haskell
  -- and __placement new__ in C++.
  destruct :: Ptr a -> IO ()

  -- | Invoke the destructor and also deallocate the storage of the object.
  -- The object must not be used after this.
  --
  -- This is equivalent to __delete expression__ in C++, so the object
  -- must be a non-array object created by a __new expression__ in C++.
  delete :: Ptr a -> IO ()

  -- | Returns the pointer to the foreign delete function, i.e. the one
  -- called by @delete@ above.
  -- This is often used as the 'FinalizerPtr' of 'ForeignPtr'.
  deleteFunPtr :: FunPtr (Ptr a -> IO ())


-- | Typeclass to read/write values to foreign objects. Implements the
-- functionality provided by @Storable@ typeclass, but for complex C++ objects.
-- The class instance must ensure that values being "poked" are correctly
-- destructed before assignment.
class Assignable a where
  -- | Write a value to given memory location. Similar to @Storable.poke@
  assign :: Ptr a -> a -> IO ()
  default assign :: (Constructible a, Destructible a) => Ptr a -> a -> IO ()
  assign p val = mask_ $ destruct p >> constructValue p val
