-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/KDF.hs: KDF module
-- Copyright (C) 2021-2024 LStandman

module Crypt.KDF
  ( hkdfSha256Expand,
    hkdfSha256Expand',
    hkdfSha256Extract,
    hkdfSha256Extract',
    hmacSha256,
    hmacSha256',
    pbkdf2HmacSha256,
    pbkdf2HmacSha256',
  )
where

import Control.Monad
--
import Crypt.SHA2
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import GHC.IO

infixl 7 `div1`

type Prf = [Word8] -> Int -> Hash

hkdfSha256Expand :: [Word8] -> Int -> [Word8] -> Int -> Int -> [Word8]
hkdfSha256Expand' :: [Word8] -> [Word8] -> Int -> [Word8]
hkdfSha256Extract :: [Word8] -> Int -> [Word8] -> Int -> [Word8]
hkdfSha256Extract' :: [Word8] -> [Word8] -> [Word8]
hmacSha256 :: [Word8] -> Int -> [Word8] -> Int -> [Word8]
hmacSha256' :: [Word8] -> [Word8] -> [Word8]
pbkdf2HmacSha256 :: [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2HmacSha256' :: [Word8] -> [Word8] -> Int -> Int -> [Word8]
div1 :: Integral a => a -> a -> a
a `div1` b = (a + b - 1) `div` b

int_hmacSha256 :: (Hash, Hash) -> [Word8] -> Int -> Hash
int_hmacSha256 (ihash, ohash) text text_size = ohash'
  where
    ihash' = int_sha256sum ihash text (sha256SizeBlock + text_size)
    ohash' =
      int_sha256sum
        ohash
        (int_sha256toList ihash')
        (sha256SizeBlock + sha256SizeDigest)

-- INFO: First block of any message is always the key _k_ after reducing and/or
--   padding it to block size. This is true for both the ihash and the ohash.
--   Therefore, we may prehash this first block and pick up from that point
--   forward for the rest of the message (the text).
-- INFO: In the case of PBKDF2, the key is constant for all rounds.
--   Prehashing once for all rounds saves us an amount of `2 * pbkdf2_rounds`
--   redundant hash calculations.
hmacSha256_prehash :: [Word8] -> Int -> (Hash, Hash)
hmacSha256_prehash k k_size = (ihash, ohash)
  where
    k'
      | k_size > sha256SizeBlock = sha256sum k k_size
      | otherwise = k
    k'' = take sha256SizeBlock $ k' ++ repeat 0
    ikey = fmap (xor 0x36) k''
    okey = fmap (xor 0x5C) k''
    ihash = int_sha256once int_sha256hash0 ikey
    ohash = int_sha256once int_sha256hash0 okey

hmacSha256 k k_size text text_size =
  int_sha256toList $
    int_hmacSha256 (hmacSha256_prehash k k_size) text text_size

hmacSha256' k text = hmacSha256 k (length k) text (length text)

int_pbkdf2HmacSha256 :: Prf -> [Word8] -> Int -> Int -> Int -> [Word8]
int_pbkdf2HmacSha256 h s s_size c dk_len =
  take dk_len $ concatMap (int_sha256toList . us . u1) [1 .. l]
  where
    split n =
      map
        fromIntegral
        [ n `shiftR` 24,
          n `shiftR` 16,
          n `shiftR` 8,
          n
        ] ::
        [Word8]
    u1 i = h (s ++ split i) (s_size + 4)
    round :: Int -> Hash -> IOUArray Int Word32 -> IO ()
    round 1 u w = return ()
    round n u w =
      case h (int_sha256toList u) sha256SizeDigest of
        u' ->
          mapM_
            (\i -> readArray w i >>= writeArray w i . xor (u' ! i))
            (range sha256BoundsHash)
            >> round (n - 1) u' w
    -- WARN: Unsafe routine used to reduce the number of intermediate result
    --   buffers for the calculation `x1 xor x2 ... xor xN`. This achieves a
    --   reduction from `N - 1` buffers down to a single buffer.
    us :: Hash -> Hash
    us u =
      unsafePerformIO $
        thaw u >>= \u' -> round c u u' >> freeze u'
    l = dk_len `div1` sha256SizeDigest

pbkdf2HmacSha256 p p_size =
  int_pbkdf2HmacSha256
    (int_hmacSha256 $! hmacSha256_prehash p p_size)

pbkdf2HmacSha256' p s =
  pbkdf2HmacSha256 p (length p) s (length s)

hkdfSha256Extract salt salt_size ikm ikm_size =
  int_sha256toList $
    (int_hmacSha256 $! hmacSha256_prehash salt salt_size) ikm ikm_size

hkdfSha256Extract' salt ikm =
  hkdfSha256Extract salt (length salt) ikm (length ikm)

int_hkdfSha256Expand :: Prf -> [Word8] -> Int -> Int -> [Word8]
int_hkdfSha256Expand h info info_size l =
  take l $ concat $ scanl' ts t1 [2 .. n]
  where
    n = l `div1` sha256SizeDigest
    t1 = int_sha256toList $ h (info ++ [1]) (info_size + 1)
    ts :: [Word8] -> Int -> [Word8]
    ts t i =
      int_sha256toList $
        h (t ++ info ++ [fromIntegral i]) (sha256SizeDigest + info_size + 1)

hkdfSha256Expand prk prk_size =
  int_hkdfSha256Expand
    (int_hmacSha256 $! hmacSha256_prehash prk prk_size)

hkdfSha256Expand' prk info =
  hkdfSha256Expand prk (length prk) info (length info)
