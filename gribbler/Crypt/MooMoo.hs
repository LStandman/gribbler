-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMoo.hs: Block cipher modes of operation
-- Copyright (C) 2021-2023 LStandman

module Crypt.MooMoo
  ( cbcEncrypt,
    cbcEncrypt1,
    cbcDecrypt,
  )
where

import Data.Bits
import Data.Word

type BlockCipher = [Word8] -> [Word8] -> [Word8]

cbcEncrypt ::
  BlockCipher -> [Word8] -> [Word8] -> [Word8] -> Int -> [Word8]
cbcEncrypt1 ::
  BlockCipher -> [Word8] -> [Word8] -> [Word8] -> Int -> ([Word8], [Word8])
cbcDecrypt ::
  BlockCipher -> [Word8] -> [Word8] -> [Word8] -> Int -> [Word8]

cbcEncrypt1 _ iv [] _ _ = ([], iv)
cbcEncrypt1 f iv ptext key block_size = (ctext1 ++ ctext2, iv_out)
  where
    (ptext1, ptext2) = splitAt block_size ptext
    ptext1' = zipWith xor ptext1 iv
    ctext1 = f ptext1' key
    (ctext2, iv_out) = cbcEncrypt1 f ctext1 ptext2 key block_size

cbcEncrypt f iv ptext key block_size =
  fst $ cbcEncrypt1 f iv ptext key block_size

cbcDecrypt _ _ [] _ _ = []
cbcDecrypt f iv ctext key block_size =
  ptext ++ cbcDecrypt f ctext1 ctext2 key block_size
  where
    (ctext1, ctext2) = splitAt block_size ctext
    ptext' = f ctext1 key
    ptext = zipWith xor ptext' iv
