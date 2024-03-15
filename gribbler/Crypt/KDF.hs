-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/KDF.hs: KDF module
-- Copyright (C) 2021-2024 LStandman

module Crypt.KDF(
    hmac,
    hmac1,
    pbkdf2,
    pbkdf2_hmac,
    pbkdf2_hmac_sha256,
    pbkdf2_hmac_sha256')
  where

import Data.Bits
import Data.List
import Data.Word
--
import Crypt.SHA2

infixl 7 `div1`

type Prf      = [Word8] -> Int -> [Word8] -> Int -> [Word8]
type Hashf    = [Word8] -> Int -> [Word8]

hmac         ::
  Hashf -> Int -> Int -> [Word8] -> Int -> [Word8] -> Int -> [Word8]
hmac1        ::
  Hashf -> Int -> Int -> [Word8] -> [Word8] -> [Word8]
pbkdf2       :: Prf -> Int -> [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac  ::
  Hashf -> Int -> Int -> [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256  ::
  [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256' ::
  [Word8] -> [Word8] -> Int -> Int -> [Word8]

div1 :: Integral a => a -> a -> a
a `div1` b = (a + b - 1) `div` b

hmac' ::
  Hashf -> Int -> Int -> [Word8] -> Int -> [Word8] -> Int -> [Word8]
hmac' h b l k _ text text_size = ohash
  where
    ipad  = take b $ repeat 0x36
    opad  = take b $ repeat 0x5C
    ihash = h (zipWith (xor) k ipad ++ text) (b + text_size)
    ohash = h (zipWith (xor) k opad ++ ihash) (b + l)

hmac_prehash ::
  Hashf -> Int -> Int -> [Word8] -> Int -> [Word8]
hmac_prehash h b l k k_size = take b (k' ++ repeat 0)
  where
    (k', k_size')
      | k_size > b  = (h k k_size, l)
      | otherwise   = (k, k_size)

hmac h b l k k_size text text_size =
  hmac' h b l (hmac_prehash h b l k k_size) b text text_size

hmac1 h b l k text = hmac (h) b l k (length k) text (length text)

pbkdf2 h h_len p p_size s s_size c dk_len =
  take dk_len $ concatMap (rehash . u1) [1..l]
  where
    split  n = map (fromIntegral) [
      n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n] :: [Word8]
    u1     i = h p p_size (s ++ split i) (s_size + 4)
    rehash' 1 u = u
    rehash' n u = case h p p_size u h_len of u' -> (zipWith (xor)) u $ rehash' (n - 1) u'
    rehash u = rehash' c u
    l        = dk_len `div1` h_len

pbkdf2_hmac h b l p p_size s s_size c dk_len =
  pbkdf2 (hmac' h b l) l (hmac_prehash h b l p p_size) b s s_size c dk_len

pbkdf2_hmac_sha256 p p_size s s_size c dk_len =
  pbkdf2_hmac
    (sha256sum) sha256_size_block sha256_size_digest
    p p_size s s_size c dk_len

pbkdf2_hmac_sha256' p s c dk_len =
  pbkdf2_hmac
    (sha256sum) sha256_size_block sha256_size_digest
    p (length p) s (length s) c dk_len
