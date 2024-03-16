-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/KDF.hs: KDF module
-- Copyright (C) 2021-2024 LStandman

module Crypt.KDF(
    hmac_sha256,
    hmac_sha256',
    pbkdf2,
    pbkdf2_hmac_sha256,
    pbkdf2_hmac_sha256')
  where

import Data.Bits
import Data.List
import Data.Word
--
import Crypt.SHA2

infixl 7 `div1`

type Prf = [Word8] -> Int -> [Word8]

hmac_sha256         :: [Word8] -> Int -> [Word8] -> Int -> [Word8]
hmac_sha256'        :: [Word8] -> [Word8] -> [Word8]
pbkdf2              :: Prf -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256  ::
  [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256' ::
  [Word8] -> [Word8] -> Int -> Int -> [Word8]

div1 :: Integral a => a -> a -> a
a `div1` b = (a + b - 1) `div` b

int_hmac_sha256 :: (Hash, Hash) -> [Word8] -> Int -> [Word8]
int_hmac_sha256 (ihash, ohash) text text_size = ohash'
  where
    ihash' = int_sha256sum ihash (text)   (sha256_size_block + text_size)
    ohash' = int_sha256sum ohash (ihash') (sha256_size_block + sha256_size_digest)

hmac_sha256_prehash :: [Word8] -> Int -> (Hash, Hash)
hmac_sha256_prehash k k_size = (ihash, ohash)
  where
    k'
      | k_size > sha256_size_block = sha256sum k k_size
      | otherwise                  = k
    k''   = take sha256_size_block $ k' ++ repeat 0
    ipad  = take sha256_size_block $ repeat 0x36
    opad  = take sha256_size_block $ repeat 0x5C
    ikey = zipWith (xor) k'' ipad
    okey = zipWith (xor) k'' opad
    ihash = int_sha256once int_sha256hash0 ikey
    ohash = int_sha256once int_sha256hash0 okey

hmac_sha256 k k_size text text_size =
  int_hmac_sha256 (hmac_sha256_prehash k k_size) text text_size

hmac_sha256' k text = hmac_sha256 k (length k) text (length text)

pbkdf2 h h_len s s_size c dk_len =
  take dk_len $ concatMap (rehash . u1) [1..l]
  where
    split  n = map (fromIntegral) [
      n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n] :: [Word8]
    u1     i = h (s ++ split i) (s_size + 4)
    rehash' 1 u = u
    rehash' n u = case h u h_len of u' -> (zipWith (xor)) u $ rehash' (n - 1) u'
    rehash u = rehash' c u
    l        = dk_len `div1` h_len

pbkdf2_hmac_sha256 p p_size s s_size c dk_len =
  pbkdf2
    (int_hmac_sha256 $! hmac_sha256_prehash p p_size)
    sha256_size_digest s s_size c dk_len

pbkdf2_hmac_sha256' p s c dk_len =
  pbkdf2_hmac_sha256 p (length p) s (length s) c dk_len
