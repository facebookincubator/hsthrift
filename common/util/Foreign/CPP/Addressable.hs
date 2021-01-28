-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE DefaultSignatures #-}
module Foreign.CPP.Addressable
  ( Addressable(..)
  , Addressable1(..)
  ) where

import Data.Complex (Complex)
import Data.Ratio (Ratio)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Foreign hiding (sizeOf, alignment)
import Foreign.C.Types
import qualified Foreign.Storable as Storable
import GHC.Fingerprint (Fingerprint)
import System.Posix.Types

class Addressable a where
  -- | Computes the storage requirements (in bytes) of the argument.
  -- The value of the argument is not used.
  sizeOf :: a -> Int
  default sizeOf :: Storable a => a -> Int
  sizeOf = Storable.sizeOf

  -- | Computes the alignment constraint of the argument. An alignment
  -- constraint @x@ is fulfilled by any address divisible by @x@.
  -- The value of the argument is not used.
  alignment :: a -> Int
  default alignment :: Storable a => a -> Int
  alignment = Storable.alignment

class Addressable1 f where
  sizeOf1 :: f a -> Int
  alignment1 :: f a -> Int

instance Addressable Bool
instance Addressable Char
instance Addressable Double
instance Addressable Float
instance Addressable Int
instance Addressable Int8
instance Addressable Int16
instance Addressable Int32
instance Addressable Int64
instance Addressable Word
instance Addressable Word8
instance Addressable Word16
instance Addressable Word32
instance Addressable Word64
instance Addressable ()
instance Addressable Fingerprint
instance Addressable CUIntMax
instance Addressable CIntMax
instance Addressable CUIntPtr
instance Addressable CIntPtr
instance Addressable CSUSeconds
instance Addressable CUSeconds
instance Addressable CTime
instance Addressable CClock
instance Addressable CSigAtomic
instance Addressable CWchar
instance Addressable CSize
instance Addressable CPtrdiff
instance Addressable CDouble
instance Addressable CFloat
instance Addressable CULLong
instance Addressable CLLong
instance Addressable CULong
instance Addressable CLong
instance Addressable CUInt
instance Addressable CInt
instance Addressable CUShort
instance Addressable CShort
instance Addressable CUChar
instance Addressable CSChar
instance Addressable CChar
instance Addressable IntPtr
instance Addressable WordPtr
instance Addressable Fd
instance Addressable CRLim
instance Addressable CTcflag
instance Addressable CSpeed
instance Addressable CCc
instance Addressable CUid
instance Addressable CNlink
instance Addressable CGid
instance Addressable CSsize
instance Addressable CPid
instance Addressable COff
instance Addressable CMode
instance Addressable CIno
instance Addressable CDev
instance (Storable a, Integral a) => Addressable (Ratio a)
instance Addressable (StablePtr a)
instance Addressable (Ptr a)
instance Addressable (FunPtr a)
instance Storable a => Addressable (Complex a)
instance Storable a => Addressable (Identity a)
instance Storable a => Addressable (Const a b)
