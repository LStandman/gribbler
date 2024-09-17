-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMoo/CBC.hs: CBC mode of operation
-- Copyright (C) 2021-2023 LStandman

module Crypt.MooMoo.CBC
  ( encrypt,
    encrypt1,
    decrypt,
  )
where

import Data.Bits
import Data.Word

type BlockCipher = [Word8] -> [Word8]

encrypt ::
  Int -> BlockCipher -> [Word8] -> [Word8] -> [Word8]
encrypt1 ::
  Int -> BlockCipher -> [Word8] -> [Word8] -> ([Word8], [Word8])
decrypt ::
  Int -> BlockCipher -> [Word8] -> [Word8] -> [Word8]

encrypt1 block_size _ iv [] = ([], iv)
encrypt1 block_size f iv ptext = (ctext1 ++ ctext2, iv_out)
  where
    (ptext1, ptext2) = splitAt block_size ptext
    ptext1' = zipWith xor ptext1 iv
    ctext1 = f ptext1'
    (ctext2, iv_out) = encrypt1 block_size f ctext1 ptext2

encrypt block_size f iv ptext =
  fst $ encrypt1 block_size f iv ptext

decrypt _ _ _ [] = []
decrypt block_size f iv ctext =
  ptext ++ decrypt block_size f ctext1 ctext2
  where
    (ctext1, ctext2) = splitAt block_size ctext
    ptext' = f ctext1
    ptext = zipWith xor ptext' iv
