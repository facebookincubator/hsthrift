module Foreign.CPP.Destructible
  ( Destructible(..)
  , newSharedPtr
  , destructAndFree
  ) where

import Foreign

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

  -- | Returns the pointer to the foreign destructor. This is often used as
  -- the 'FinalizerPtr' of 'ForeignPtr'.
  deleteFunPtr :: FunPtr (Ptr a -> IO ())

-- | Returns a 'ForeignPtr' that owns and manages a foreign object through
-- a pointer and disposes of that object when the lifetime of the 'ForeignPtr'
-- ends.
newSharedPtr :: Destructible a => Ptr a -> IO (ForeignPtr a)
newSharedPtr = newForeignPtr deleteFunPtr

-- | Simply calls 'destruct' and 'free'. This should be used instead of
-- 'delete' when the memory is allocated by 'malloc' or 'mallocBytes' in
-- Haskell and the object is constructed by __placement new__ in C++.
destructAndFree :: Destructible a => Ptr a -> IO ()
destructAndFree p = destruct p >> free p
