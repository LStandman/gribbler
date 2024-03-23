-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/KDF.hs: KDF module
-- Copyright (C) 2021-2024 LStandman

module Crypt.KDF(
    hmac_sha256,
    hmac_sha256',
    pbkdf2_hmac_sha256,
    pbkdf2_hmac_sha256')
  where

import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import GHC.IO
--
import Crypt.SHA2

infixl 7 `div1`

type Prf = [Word8] -> Int -> Hash

hmac_sha256         :: [Word8] -> Int -> [Word8] -> Int -> [Word8]
hmac_sha256'        :: [Word8] -> [Word8] -> [Word8]
pbkdf2_hmac_sha256  :: [Word8] -> Int -> [Word8] -> Int -> Int -> Int -> [Word8]
pbkdf2_hmac_sha256' :: [Word8] -> [Word8] -> Int -> Int -> [Word8]

div1 :: Integral a => a -> a -> a
a `div1` b = (a + b - 1) `div` b

int_hmac_sha256 :: (Hash, Hash) -> [Word8] -> Int -> Hash
int_hmac_sha256 (ihash, ohash) text text_size = ohash'
  where
    ihash' = int_sha256sum ihash text (sha256_size_block + text_size)
    ohash' =
      int_sha256sum
        ohash
        (int_sha256toList ihash')
        (sha256_size_block + sha256_size_digest)

-- INFO: First block of any message is always the key _k_ after reducing and/or
--   padding it to block size. This is true for both the ihash and the ohash.
--   Therefore, we may prehash this first block and pick up from that point
--   forward for the rest of the message (the text).
-- INFO: In the case of PBKDF2, the key is constant for all rounds. 
--   Prehashing once for all rounds saves us an amount of `2 * pbkdf2_rounds`
--   redundant hash calculations.
hmac_sha256_prehash :: [Word8] -> Int -> (Hash, Hash)
hmac_sha256_prehash k k_size = (ihash, ohash)
  where
    k'
      | k_size > sha256_size_block = sha256sum k k_size
      | otherwise                  = k
    k''   = take sha256_size_block $ k' ++ repeat 0
    ikey = fmap (xor 0x36) k''
    okey = fmap (xor 0x5C) k''
    ihash = int_sha256once int_sha256hash0 ikey
    ohash = int_sha256once int_sha256hash0 okey

hmac_sha256 k k_size text text_size =
  int_sha256toList $
  int_hmac_sha256 (hmac_sha256_prehash k k_size) text text_size

hmac_sha256' k text = hmac_sha256 k (length k) text (length text)

int_pbkdf2_hmac_sha256 :: Prf -> [Word8] -> Int -> Int -> Int -> [Word8]
int_pbkdf2_hmac_sha256 h s s_size c dk_len =
  take dk_len $ concatMap (int_sha256toList . us . u1) [1..l]
  where
    split n = map (fromIntegral) [
      n `shiftR` 24, n `shiftR` 16, n `shiftR` 8, n] :: [Word8]
    u1 i = h (s ++ split i) (s_size + 4)
    round :: Int -> Hash -> IOUArray Int Word32 -> IO ()
    round 1 u w = return ()
    round n u w =
      case h (int_sha256toList u) sha256_size_digest of
        u' ->
          ( mapM_ (\ i -> readArray w i >>= (writeArray w i) . (xor (u'!i))) $
            range sha256_bounds_hash) >>
          round (n - 1) u' w
    -- WARN: Unsafe routine used to reduce the number of intermediate result
    --   buffers for the calculation `x1 xor x2 ... xor xN`. This achieves a
    --   reduction from `N - 1` buffers down to a single buffer.
    us :: Hash -> Hash
    us u =
      unsafePerformIO $
      thaw u >>= \ u' -> round c u u' >> freeze u'
    l = dk_len `div1` sha256_size_digest

pbkdf2_hmac_sha256 p p_size s s_size c dk_len =
  int_pbkdf2_hmac_sha256
    (int_hmac_sha256 $! hmac_sha256_prehash p p_size)
    s
    s_size
    c
    dk_len

pbkdf2_hmac_sha256' p s c dk_len =
  pbkdf2_hmac_sha256 p (length p) s (length s) c dk_len
