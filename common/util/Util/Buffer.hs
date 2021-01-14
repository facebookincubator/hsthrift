-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

-- | Byte buffers in ST with amortized O(1) append. They are significantly
-- faster and more memory efficient than things built on top of lazy
-- bytestrings.

module Util.Buffer
  ( Buffer, Fill
  , liftST
  , alloc
  , ascii
  , byte
  , byteString
  , fillByteString
  )
where

import Control.Exception (assert)
import Control.Monad.Primitive
import Control.Monad.ST (ST)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Char (isAscii)
#if !MIN_VERSION_primitive(0,7,0)
import Data.Primitive.Addr (Addr(..))
#endif
import Data.Primitive.ByteArray
import GHC.Exts
  ( Int(..), plusAddr#
  , copyAddrToByteArray#, shrinkMutableByteArray#
  , unsafeCoerce#
  )
import GHC.ForeignPtr
import GHC.Ptr (Ptr(..), plusPtr)
import GHC.Word (Word8(..))

-- | An exponentially growing buffer of bytes in ST with amortized O(1) append.
--
-- Invariants:
--
-- > bufLen buf <= bufRes buf
-- > bufRes buf <= bufCap buf
-- > bufCap buf > 0
--
data Buffer s = Buffer
  { -- | The actual bytes
    bufData :: {-# UNPACK #-} !(MutableByteArray s)

    -- | How much of the buffer has been filled
  , bufLen :: {-# UNPACK #-} !Int

    -- | How much of the buffer has been reserved
  , bufRes :: {-# UNPACK #-} !Int

    -- | Capacity of the buffer (i.e., length of the MutableByteArray)
  , bufCap :: {-# UNPACK #-} !Int
  }

-- | An ST-based monad for filling.
data Fill s a = Fill
  { -- | How many bytes to reserve - _fillIt assumes that the Buffer will have
    -- at least that much free space. This helps avoid repeated calls to
    -- 'reserve'.
    _fillRes :: {-# UNPACK #-} !Int

    -- | The function that fills the Buffer. The old Buffer may not be used
    -- after this function has been invoked.
  , _fillIt :: Buffer s -> ST s (Buffer s, a)
  }

instance Functor (Fill s) where
  {-# INLINE fmap #-}
  fmap f (Fill n h) = Fill n $ fmap (fmap (fmap f)) h

instance Applicative (Fill s) where
  {-# INLINE pure #-}
  pure x = Fill 0 $ \ !buf -> return (buf, x)

  {-# INLINE (<*>) #-}
  Fill m f <*> Fill n g = Fill (m+n) $ \ !buf -> do
    (!buf, h) <- f buf
    (!buf, x) <- g buf
    return (buf, h x)

instance Monad (Fill s) where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  Fill m f >>= g = Fill m $ \ !buf -> do
    (!buf, x) <- f buf
    fill buf $ g x

  {-# INLINE (>>) #-}
  Fill m f >> Fill n g = Fill (m+n) $ \ !buf -> do
    (!buf,_) <- f buf
    g buf

-- | Fill a buffer. The old buffer may not be used after this.
fill :: Buffer s -> Fill s a -> ST s (Buffer s, a)
{-# INLINE fill #-}
fill !buf (Fill n f) = do
  buf <- reserve buf n
  f buf

-- | Lift 'ST' actions into 'Fill'.
liftST :: ST s a -> Fill s a
{-# INLINE liftST #-}
liftST st = Fill 0 $ \ !buf -> (,) buf <$> st

-- | Write into the buffer.
write
  :: Int -- ^ max number of bytes to write
  -> (MutableByteArray s -> Int -> ST s Int)
    -- ^ takes the raw array and an initial offset and returns the offset
    -- one past the last byte it wrote
  -> Fill s ()
{-# INLINE write #-}
write n f = Fill n $ \(Buffer arr len res cap) -> do
  len' <- f arr len
  return (Buffer arr len' res cap, ())

-- | Write an ASCII (<=127) character into the buffer.
ascii :: Char -> Fill s ()
{-# INLINE ascii #-}
ascii c = assert (isAscii c) $ byte $ fromIntegral $ fromEnum c

-- | Write a byte into the buffer.
byte :: Word8 -> Fill s ()
{-# INLINE byte #-}
byte !x = write 1 $ \arr i -> do
  writeByteArray arr i x
  return (i+1)

-- | Write a 'ByteString' into the buffer.
byteString :: ByteString -> Fill s ()
{-# INLINE byteString #-}
byteString (BSI.PS (ForeignPtr addr# r) (I# k#) n@(I# n#)) =
  write n $ \(MutableByteArray arr#) i@(I# i#) -> do
    -- primitive doesn't wrap this so no point in trying to make things nice
    primitive_ (copyAddrToByteArray# (plusAddr# addr# k#) arr# i# n#)
    touch r
    return (i+n)

-- | Write into the buffer using a pointer. Writing more than the max number of
-- bytes will cause segfaults!
alloc
  :: Int -- ^ max number of bytes to write
  -> (forall s. Ptr Word8 -> ST s Int)
    -- ^ takes a pointer to the free space and returns the number of bytes
    -- written
  -> Fill s ()
{-# INLINE alloc #-}
alloc !n f = write n $ \arr i -> do
#if MIN_VERSION_primitive(0,7,0)
  let p = mutableByteArrayContents arr
  k <- f (p `plusPtr` i)
#else
  let !(Addr addr#) = mutableByteArrayContents arr
  k <- f (Ptr addr# `plusPtr` i)
#endif
  assert (k <= n) $ return ()
  touch arr
  return (i+k)

-- | Fill a buffer and turn it into a 'ByteString'.
fillByteString
  :: Int -- ^ initial capacity
  -> Fill s () -- ^ data
  -> ST s ByteString
{-# INLINE fillByteString #-}
fillByteString n f = do
  buf <- new n
  (buf, _) <- fill buf f
  unsafeFreezeByteString buf

-- | Create a new buffer with the given initial capacity.
new :: Int -> ST s (Buffer s)
new !cap = do
  let !c = max cap 1
  arr <- newPinnedByteArray c
  return Buffer
    { bufData = arr
    , bufLen = 0
    , bufRes = 0
    , bufCap = c
    }

reserve :: Buffer s -> Int -> ST s (Buffer s)
reserve (Buffer arr len res cap) !n
  | wanted <= cap = return $ Buffer arr len wanted cap
  | otherwise = do
      let !bcap = cap + max cap (wanted - cap)
      brr <- resize arr len bcap
      return $ Buffer brr len wanted bcap
  where
    !wanted = res + n

unsafeFreezeByteString :: Buffer s -> ST s ByteString
unsafeFreezeByteString (Buffer arr@(MutableByteArray arr#) len _ _) = do
  shrink arr len
  case mutableByteArrayContents arr of
#if MIN_VERSION_primitive(0,7,0)
    Ptr addr# ->
#else
    Addr addr# ->
#endif
      return $ BSI.fromForeignPtr
        (ForeignPtr addr# (PlainPtr (unsafeCoerce# arr#)))
        0
        len

resize :: MutableByteArray s -> Int -> Int -> ST s (MutableByteArray s)
{-# INLINE resize #-}
resize arr len cap = do
  brr <- newPinnedByteArray cap
  copyMutableByteArray brr 0 arr 0 len
  return brr

shrink :: MutableByteArray s -> Int -> ST s ()
shrink (MutableByteArray arr#) (I# n#) =
  -- shrinkMutableByteArray isn't wrapped by primitive
  -- NOTE: we assume that shrinking never unpins
  primitive_ (shrinkMutableByteArray# arr# n#)
