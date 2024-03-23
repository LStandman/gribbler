-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/SHA2.hs: SHA2 module
-- Copyright (C) 2021-2024 LStandman

module Crypt.SHA2(
    Hash,
    int_sha256hash0,
    int_sha256once,
    int_sha256sum,
    int_sha256toList,
    sha256_bounds_hash,
    sha256_size_block,
    sha256_size_digest,
    sha256_size_hash,
    sha256sum,
    sha256sum1)
  where

import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import GHC.IO

type Hash = UArray Int Word32

int_sha256hash0    :: Hash
int_sha256once     :: Hash -> [Word8] -> Hash
int_sha256sum      :: Hash -> [Word8] -> Int -> Hash
int_sha256toList   :: Hash -> [Word8]
sha256_bounds_hash :: (Int, Int)
sha256_size_block  :: Int
sha256_size_digest :: Int
sha256_size_hash   :: Int
sha256sum          :: [Word8] -> Int -> [Word8]
sha256sum1         :: [Word8] -> [Word8]

sha256_size_block  = 64
sha256_size_digest = 32
sha256_size_hash   = 8
size_block         = 16

sha256_bounds_hash = (0, sha256_size_hash - 1)

int_sha256hash0 = listArray sha256_bounds_hash [
  0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
  0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19] :: Hash

ch :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` ((complement x) .&. z)

maj :: Word32 -> Word32 -> Word32 -> Word32
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

big_sigma0 :: Word32 -> Word32
big_sigma0 x = (x `rotateR` 2) `xor` (x `rotateR` 13) `xor` (x `rotateR` 22)

big_sigma1 :: Word32 -> Word32
big_sigma1 x = (x `rotateR` 6) `xor` (x `rotateR` 11) `xor` (x `rotateR` 25)

lil_sigma0 :: Word32 -> Word32
lil_sigma0 x = (x `rotateR` 7) `xor` (x `rotateR` 18) `xor` (x `shiftR` 3)

lil_sigma1 :: Word32 -> Word32
lil_sigma1 x = (x `rotateR` 17) `xor` (x `rotateR` 19) `xor` (x `shiftR` 10)

sha256round :: Hash -> Word32 -> Hash
sha256round v x = w
  where
    a  = v!0
    b  = v!1
    c  = v!2
    d  = v!3
    e  = v!4
    f  = v!5
    g  = v!6
    h  = v!7
    t1 = h + (big_sigma1 e) + (ch e f g) + x
    t2 = (big_sigma0 a) + (maj a b c)
    u  = ixmap sha256_bounds_hash (\ i -> (i - 1) `mod` sha256_size_hash) v
    w  = u//[(0, t1 + t2), (4, d + t1)]


sha256block :: Hash -> [Word32] -> Hash
sha256block h v =
    array sha256_bounds_hash [(i, h!i + h'!i) | i <- range sha256_bounds_hash]
  where
    h' = foldl' (sha256round) h v

k  = listArray (0, 63) [
  0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5,
  0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
  0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3,
  0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
  0xE49B69C1, 0xEFBE4786, 0x0FC19DC6, 0x240CA1CC,
  0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
  0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7,
  0xC6E00BF3, 0xD5A79147, 0x06CA6351, 0x14292967,
  0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13,
  0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
  0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3,
  0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
  0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5,
  0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
  0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208,
  0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2] :: UArray Int Word32

sha256sched' :: IOUArray Int Word32 -> Int -> IO ()
sha256sched' v i = 
  readArray v (i - 2) >>=
  \ a -> readArray v (i - 7) >>=
    \ b -> readArray v (i - 15) >>=
      \ c -> readArray v (i - 16) >>=
        \ d ->
          writeArray v i (lil_sigma1 a  + b + lil_sigma0 c + d)

sha256sched :: IOUArray Int Word32 -> IO ()
sha256sched v =
  mapM_ (sha256sched' v) [16..63] >>
  mapM_ (\ i -> readArray v i >>= \ a -> writeArray v i (a + k!i)) [0..63]

from_list :: [Word8] -> [Word32]
from_list [] = []
from_list m  = foldl' (f) 0 m1 : from_list m2
  where
    f a b    = (a `shiftL` 8) .|. fromIntegral b
    (m1, m2) = splitAt 4 m

int_sha256once h [] = h
int_sha256once h m  = h'
  where
    w  = unsafePerformIO $
         newListArray (0, 63) (from_list m) >>=
         \ v -> sha256sched v >>
         getElems v
    h' = sha256block h w

sha256sum' :: Hash -> [Word8] -> Int -> Hash
sha256sum' h m l =
  case splitAt sha256_size_block m of
    (m1, []) ->
      case splitAt sha256_size_block
        (m1 ++ [0x80] ++ (take n $ repeat 0) ++ split (l * 8)) of
          (m1', m2') -> int_sha256once (int_sha256once h m1') m2'
    (m1, m2) -> sha256sum' (int_sha256once h m1) m2 l
  where
    split n = map (fromIntegral) [
      n `shiftR` 56, n `shiftR` 48, n `shiftR` 40, n `shiftR` 32,
      n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n] :: [Word8]
    n       = (sha256_size_block - 8 - (l + 1)) `mod` sha256_size_block

int_sha256toList h =
  concatMap (split) $ elems h
  where
    split n = map (fromIntegral)
      [n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n] :: [Word8]

int_sha256sum h m l = sha256sum' h m l

sha256sum m l = int_sha256toList $ int_sha256sum int_sha256hash0 m l

sha256sum1 m = sha256sum m (length m)
