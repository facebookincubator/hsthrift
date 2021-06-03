-- Copyright 2004-present Facebook. All Rights Reserved.

module Foreign.CPP.HsStruct.Utils
  ( withCxxObject
  , withDefaultCxxObject
  ) where


import Control.Exception (bracket)
import Foreign (Ptr)
import Foreign.CPP.Addressable
import Foreign.CPP.Marshallable

import Util.Memory

-- | allocates space for an object, calls `constructValue` with a value
-- and `destruct`s the object when done.
withCxxObject
  :: (Constructible a, Destructible a, Addressable a)
  => a
  -> (Ptr a -> IO b)
  -> IO b
withCxxObject val func =
  allocaBytesAligned (sizeOf val) (alignment val) $ \s ->
    bracket (constructValue s val >> return s) destruct func

-- | allocates space for an object, calls `constructDefault`
-- and `destruct`s the object when done.
withDefaultCxxObject
  :: forall a b . (DefaultConstructible a, Destructible a, Addressable a)
  => (Ptr a -> IO b)
  -> IO b
withDefaultCxxObject func =
  allocaBytesAligned (sizeOf val) (alignment val) $ \s ->
    bracket (constructDefault s >> return s) destruct func
  where
    val = undefined :: a
