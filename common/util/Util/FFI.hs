{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeOperators #-}
module Util.FFI (
  FFIError, ffiErrorMessage, call,
  List(..), FFIFun(..), Tuple(..), invoke,
) where

import Foreign hiding (with, withMany)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import Control.Monad

foreign import ccall unsafe "&hs_ffi_free_error" hs_ffi_free_error
  :: FunPtr (CString -> IO ())

newtype FFIError = FFIError (ForeignPtr CChar)

ffiErrorMessage :: FFIError -> String
ffiErrorMessage (FFIError fp) =
  unsafePerformIO $ withForeignPtr fp peekCString

instance Show FFIError where
  show = ffiErrorMessage

instance Exception FFIError

-- | Call an FFI function that returns 'nullPtr' if it succeeds, or a
-- 'CString' containing the error message if it fails.  On failure the
-- error is thrown as an 'FFIError' exception.
call :: IO CString -> IO ()
call f = do
  p <- f
  when (p /= nullPtr) $ do
    fp <- newForeignPtr hs_ffi_free_error p
    throwIO $ FFIError fp

infixr 5 :>

data List xs where
  Nil :: List '[]
  (:>) :: x -> List xs -> List (x ': xs)

class Tuple xs where
  type Tup xs
  tuple :: List xs -> Tup xs

instance Tuple '[] where
  type Tup '[] = ()
  tuple Nil = ()

instance Tuple '[a] where
  type Tup '[a] = a
  tuple (a :> _) = a

instance Tuple [a,b] where
  type Tup [a,b] = (a,b)
  tuple (a :> b :> _) = (a,b)

instance Tuple [a,b,c] where
  type Tup [a,b,c] = (a,b,c)
  tuple (a :> b :> c :> _) = (a,b,c)

instance Tuple [a,b,c,d] where
  type Tup [a,b,c,d] = (a,b,c,d)
  tuple (a :> b :> c :> d :> _) = (a,b,c,d)

instance Tuple [a,b,c,d,e] where
  type Tup [a,b,c,d,e] = (a,b,c,d,e)
  tuple (a :> b :> c :> d :> e :> _) = (a,b,c,d,e)

instance Tuple [a,b,c,d,e,f] where
  type Tup [a,b,c,d,e,f] = (a,b,c,d,e,f)
  tuple (a :> b :> c :> d :> e :> f :> _) = (a,b,c,d,e,f)

instance Tuple [a,b,c,d,e,f,g] where
  type Tup [a,b,c,d,e,f,g] = (a,b,c,d,e,f,g)
  tuple (a :> b :> c :> d :> e :> f :> g :> _) = (a,b,c,d,e,f,g)

instance Tuple [a,b,c,d,e,f,g,h] where
  type Tup [a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h)
  tuple (a :> b :> c :> d :> e :> f :> g :> h :> _) = (a,b,c,d,e,f,g,h)

instance Tuple [a,b,c,d,e,f,g,h,i] where
  type Tup [a,b,c,d,e,f,g,h,i] = (a,b,c,d,e,f,g,h,i)
  tuple (a :> b :> c :> d :> e :> f :> g :> h :> i :> _) = (a,b,c,d,e,f,g,h,i)

class FFIFun f where
  type Res f :: [*]
  invoke' :: f -> IO (List (Res f))

instance FFIFun (IO CString) where
  type Res (IO CString) = '[]
  invoke' f = do
    call f
    return Nil

instance FFIFun (IO ()) where
  type Res (IO ()) = '[]
  invoke' f = do
    f
    return Nil

-- | For output parameter 'Ptr a' this will 'alloca' memory (unitialized)
-- and then proceed to the next parameter.  On returning this prepends
-- the result into x.  This is only safe if the invoked function
-- always fills in this output parameter pointer before returning.
instance (Storable a, FFIFun f) => FFIFun (Ptr a -> f) where
  type Res (Ptr a -> f) = a ': Res f
  invoke' f = alloca $ \p -> do
    xs <- invoke' (f p)
    x <- peek p
    return $ x :> xs

-- | Call an FFI function with automatic handling of return values and
-- errors.
--
-- For example, if we have a function with one input and two outputs,
-- optionally returning an error as a `CString`:
--
-- > foreign import ccall unsafe json_encode
-- >   :: Ptr Dyn -> Ptr (Ptr Word8) -> Ptr CSize -> IO CString
--
-- Then we can call it like this:
--
-- >   (bytes, size) <- invoke $ json_encode dyn
--
-- where @bytes :: Ptr Word8@ and @size :: CSize@.
--
-- Note: this is only safe if the invoked function always fills in
-- output parameter pointers before returning.
invoke :: (FFIFun f, Tuple (Res f)) => f -> IO (Tup (Res f))
invoke = fmap tuple . invoke'
