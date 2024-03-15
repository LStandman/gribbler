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

type Prf      = [Word8] -> Int -> [Word8]
type Hashf    = [Word8] -> Int -> [Word8]

hmac         ::
  Hashf -> Int -> Int -> [Word8] -> Int -> [Word8] -> Int -> [Word8]
hmac1        ::
  Hashf -> Int -> Int -> [Word8] -> [Word8] -> [Word8]
pbkdf2       :: Prf -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac  ::
  Hashf -> Int -> Int -> [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256  ::
  [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256' ::
  [Word8] -> [Word8] -> Int -> Int -> [Word8]

div1 :: Integral a => a -> a -> a
a `div1` b = (a + b - 1) `div` b

hmac' :: Hashf -> Int -> Int -> ([Word8], [Word8]) -> [Word8] -> Int -> [Word8]
hmac' h b l (ikey, okey) text text_size = ohash
  where
    ihash = h (ikey ++ text) (b + text_size)
    ohash = h (okey ++ ihash) (b + l)

hmac_warm_keys :: Hashf -> Int -> Int -> [Word8] -> Int -> ([Word8], [Word8])
hmac_warm_keys h b l k k_size = (ikey, okey)
  where
    k'
      | k_size > b  = h k k_size
      | otherwise   = k
    k''   = take b $ k' ++ repeat 0
    ipad  = take b $ repeat 0x36
    opad  = take b $ repeat 0x5C
    ikey = zipWith (xor) k'' ipad
    okey = zipWith (xor) k'' opad

hmac h b l k k_size text text_size = hmac' h b l (hmac_warm_keys h b l k k_size) text text_size

hmac1 h b l k text = hmac (h) b l k (length k) text (length text)

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

pbkdf2_hmac h b l p p_size s s_size c dk_len =
  pbkdf2 (hmac' h b l $! hmac_warm_keys h b l p p_size) l s s_size c dk_len

pbkdf2_hmac_sha256 p p_size s s_size c dk_len =
  pbkdf2_hmac
    (sha256sum) sha256_size_block sha256_size_digest
    p p_size s s_size c dk_len

pbkdf2_hmac_sha256' p s c dk_len =
  pbkdf2_hmac
    (sha256sum) sha256_size_block sha256_size_digest
    p (length p) s (length s) c dk_len
