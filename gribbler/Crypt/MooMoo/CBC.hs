-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMoo/CBC.hs: Block cipher modes of operation
-- Copyright (C) 2021-2023 LStandman

module Crypt.MooMoo.CBC
  ( encrypt,
    encrypt1,
    decrypt,
  )
where

import Data.Bits
import Data.Word

type BlockCipher = [Word8] -> [Word8] -> [Word8]

encrypt ::
  BlockCipher -> [Word8] -> [Word8] -> [Word8] -> Int -> [Word8]
encrypt1 ::
  BlockCipher -> [Word8] -> [Word8] -> [Word8] -> Int -> ([Word8], [Word8])
decrypt ::
  BlockCipher -> [Word8] -> [Word8] -> [Word8] -> Int -> [Word8]

encrypt1 _ iv [] _ _ = ([], iv)
encrypt1 f iv ptext key block_size = (ctext1 ++ ctext2, iv_out)
  where
    (ptext1, ptext2) = splitAt block_size ptext
    ptext1' = zipWith xor ptext1 iv
    ctext1 = f ptext1' key
    (ctext2, iv_out) = encrypt1 f ctext1 ptext2 key block_size

encrypt f iv ptext key block_size =
  fst $ encrypt1 f iv ptext key block_size

decrypt _ _ [] _ _ = []
decrypt f iv ctext key block_size =
  ptext ++ decrypt f ctext1 ctext2 key block_size
  where
    (ctext1, ctext2) = splitAt block_size ctext
    ptext' = f ctext1 key
    ptext = zipWith xor ptext' iv
