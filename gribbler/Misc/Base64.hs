-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64.hs: Base64 implementation
-- Copyright (C) 2024 LStandman

module Misc.Base64(
    Alphabet,
    encode)
  where

import Data.Array.Unboxed
import Data.Bits
import Data.Word

type Alphabet = UArray (Word8) Char

encode :: Alphabet -> (Word8, Word8, Word8) -> (Char, Char, Char, Char)
--decode :: String -> [Word8]

encode alphabet (a, b, c) =
  ( alphabet!(a `shiftR` 2),
    alphabet!((a `shiftL` 4 .|. b `shiftR` 4) .&. 0x3F),
    alphabet!((b `shiftL` 2 .|. c `shiftR` 6) .&. 0x3F),
    alphabet!(c .&. 0x3F))
