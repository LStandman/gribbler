-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/SHA512.hs: SHA512 module
-- Copyright (C) 2024 LStandman

module Crypt.SHA512
  ( Hash,
    int_sha512hash0,
    int_sha512once,
    int_sha512sum,
    int_sha512toList,
    sha512BoundsHash,
    sha512SizeBlock,
    sha512SizeDigest,
    sha512SizeHash,
    sha512sum,
    sha512sum1,
  )
where

import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import GHC.IO

type Hash = UArray Int Word64

int_sha512hash0 :: Hash
int_sha512once :: Hash -> [Word8] -> Hash
int_sha512sum :: Hash -> [Word8] -> Int -> Hash
int_sha512toList :: Hash -> [Word8]
sha512BoundsHash :: (Int, Int)
sha512SizeBlock :: Int
sha512SizeDigest :: Int
sha512SizeHash :: Int
sha512sum :: [Word8] -> Int -> [Word8]
sha512sum1 :: [Word8] -> [Word8]

sha512SizeBlock = 128

sha512SizeDigest = 64

sha512SizeHash = 8

sizeBlock = 16

sha512BoundsHash = (0, sha512SizeHash - 1)

{- ORMOLU_DISABLE -}
int_sha512hash0 =
  listArray
    sha512BoundsHash
    [ 0x6A09E667F3BCC908, 0xBB67AE8584CAA73B,
      0x3C6EF372FE94F82B, 0xA54FF53A5F1D36F1,
      0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
      0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179
    ] ::
    Hash
{- ORMOLU_ENABLE -}

ch :: Word64 -> Word64 -> Word64 -> Word64
ch x y z = (x .&. y) `xor` (complement x .&. z)

maj :: Word64 -> Word64 -> Word64 -> Word64
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

bigSigma0 :: Word64 -> Word64
bigSigma0 x = (x `rotateR` 28) `xor` (x `rotateR` 34) `xor` (x `rotateR` 39)

bigSigma1 :: Word64 -> Word64
bigSigma1 x = (x `rotateR` 14) `xor` (x `rotateR` 18) `xor` (x `rotateR` 41)

lilSigma0 :: Word64 -> Word64
lilSigma0 x = (x `rotateR` 1) `xor` (x `rotateR` 8) `xor` (x `shiftR` 7)

lilSigma1 :: Word64 -> Word64
lilSigma1 x = (x `rotateR` 19) `xor` (x `rotateR` 61) `xor` (x `shiftR` 6)

sha512round :: Hash -> Word64 -> Hash
sha512round v x =
  array
    sha512BoundsHash
    [ (0, t1 + t2),
      (1, a),
      (2, b),
      (3, c),
      (4, d + t1),
      (5, e),
      (6, f),
      (7, g)
    ]
  where
    a = v ! 0
    b = v ! 1
    c = v ! 2
    d = v ! 3
    e = v ! 4
    f = v ! 5
    g = v ! 6
    h = v ! 7
    t1 = h + bigSigma1 e + ch e f g + x
    t2 = bigSigma0 a + maj a b c

sha512block :: Hash -> [Word64] -> Hash
sha512block h v =
  array sha512BoundsHash [(i, h ! i + h' ! i) | i <- range sha512BoundsHash]
  where
    h' = foldl' sha512round h v

{- ORMOLU_DISABLE -}
k =
  listArray
    (0, 79)
    [ 0x428A2F98D728AE22, 0x7137449123EF65CD,
      0xB5C0FBCFEC4D3B2F, 0xE9B5DBA58189DBBC,
      0x3956C25BF348B538, 0x59F111F1B605D019,
      0x923F82A4AF194F9B, 0xAB1C5ED5DA6D8118,
      0xD807AA98A3030242, 0x12835B0145706FBE,
      0x243185BE4EE4B28C, 0x550C7DC3D5FFB4E2,
      0x72BE5D74F27B896F, 0x80DEB1FE3B1696B1,
      0x9BDC06A725C71235, 0xC19BF174CF692694,
      0xE49B69C19EF14AD2, 0xEFBE4786384F25E3,
      0x0FC19DC68B8CD5B5, 0x240CA1CC77AC9C65,
      0x2DE92C6F592B0275, 0x4A7484AA6EA6E483,
      0x5CB0A9DCBD41FBD4, 0x76F988DA831153B5,
      0x983E5152EE66DFAB, 0xA831C66D2DB43210,
      0xB00327C898FB213F, 0xBF597FC7BEEF0EE4,
      0xC6E00BF33DA88FC2, 0xD5A79147930AA725,
      0x06CA6351E003826F, 0x142929670A0E6E70,
      0x27B70A8546D22FFC, 0x2E1B21385C26C926,
      0x4D2C6DFC5AC42AED, 0x53380D139D95B3DF,
      0x650A73548BAF63DE, 0x766A0ABB3C77B2A8,
      0x81C2C92E47EDAEE6, 0x92722C851482353B,
      0xA2BFE8A14CF10364, 0xA81A664BBC423001,
      0xC24B8B70D0F89791, 0xC76C51A30654BE30,
      0xD192E819D6EF5218, 0xD69906245565A910,
      0xF40E35855771202A, 0x106AA07032BBD1B8,
      0x19A4C116B8D2D0C8, 0x1E376C085141AB53,
      0x2748774CDF8EEB99, 0x34B0BCB5E19B48A8,
      0x391C0CB3C5C95A63, 0x4ED8AA4AE3418ACB,
      0x5B9CCA4F7763E373, 0x682E6FF3D6B2B8A3,
      0x748F82EE5DEFB2FC, 0x78A5636F43172F60,
      0x84C87814A1F0AB72, 0x8CC702081A6439EC,
      0x90BEFFFA23631E28, 0xA4506CEBDE82BDE9,
      0xBEF9A3F7B2C67915, 0xC67178F2E372532B,
      0xCA273ECEEA26619C, 0xD186B8C721C0C207,
      0xEADA7DD6CDE0EB1E, 0xF57D4F7FEE6ED178,
      0x06F067AA72176FBA, 0x0A637DC5A2C898A6,
      0x113F9804BEF90DAE, 0x1B710B35131C471B,
      0x28DB77F523047D84, 0x32CAAB7B40C72493,
      0x3C9EBE0A15C9BEBC, 0x431D67C49C100D4C,
      0x4CC5D4BECB3E42B6, 0x597F299CFC657E2A,
      0x5FCB6FAB3AD6FAEC, 0x6C44198C4A475817
    ] ::
    UArray Int Word64
{- ORMOLU_ENABLE -}

sha512sched' :: IOUArray Int Word64 -> Int -> IO ()
sha512sched' v i =
  readArray v (i - 2)
    >>= \a ->
      readArray v (i - 7)
        >>= \b ->
          readArray v (i - 15)
            >>= \c ->
              readArray v (i - 16)
                >>= \d ->
                  writeArray v i (lilSigma1 a + b + lilSigma0 c + d)

sha512sched :: IOUArray Int Word64 -> IO ()
sha512sched v =
  mapM_ (sha512sched' v) [16 .. 79]
    >> mapM_ (\i -> readArray v i >>= writeArray v i . (+ k ! i)) [0 .. 79]

fromList :: [Word8] -> [Word64]
fromList [] = []
fromList m = foldl' f 0 m1 : fromList m2
  where
    f a b = (a `shiftL` 8) .|. fromIntegral b
    (m1, m2) = splitAt 8 m

int_sha512once h m = h'
  where
    -- WARN: Unsafe routine used to expand the schedule in place.
    --   Besides the speedy indexing/deindexing, this saves us 24 intermediate
    --   result buffers (48 cells, computed up to 2 at a time).
    -- INFO: `k + w` is also unthunked herein to reduce memory consumption.
    w =
      unsafePerformIO $
        newListArray (0, 79) (fromList m)
          >>= \v ->
            sha512sched v
              >> getElems v
    h' = sha512block h w

sha512sum' :: Hash -> [Word8] -> Int -> Hash
sha512sum' h m l =
  case splitAt sha512SizeBlock m of
    (m1, []) ->
      case splitAt
        sha512SizeBlock
        (m1 ++ [0x80] ++ replicate n 0 ++ split (l * 8)) of
        (m1', []) -> int_sha512once h m1'
        (m1', m2') -> int_sha512once (int_sha512once h m1') m2'
    (m1, m2) -> sha512sum' (int_sha512once h m1) m2 l
  where
    split x =
      map
        fromIntegral
        [ x `shiftR` 120,
          x `shiftR` 112,
          x `shiftR` 104,
          x `shiftR` 96,
          x `shiftR` 88,
          x `shiftR` 80,
          x `shiftR` 72,
          x `shiftR` 64,
          x `shiftR` 56,
          x `shiftR` 48,
          x `shiftR` 40,
          x `shiftR` 32,
          x `shiftR` 24,
          x `shiftR` 16,
          x `shiftR` 8,
          x
        ] ::
        [Word8]
    n = (sha512SizeBlock - 16 - (l + 1)) `mod` sha512SizeBlock

int_sha512toList h =
  concatMap split $ elems h
  where
    split x =
      map
        fromIntegral
        [ x `shiftR` 56,
          x `shiftR` 48,
          x `shiftR` 40,
          x `shiftR` 32,
          x `shiftR` 24,
          x `shiftR` 16,
          x `shiftR` 8,
          x
        ] ::
        [Word8]

int_sha512sum = sha512sum'

sha512sum m l = int_sha512toList $ int_sha512sum int_sha512hash0 m l

sha512sum1 m = sha512sum m (length m)
