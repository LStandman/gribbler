-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64/RFC4648.hs: Base64 RFC4648 implementation.
-- Copyright (C) 2024 LStandman

module Misc.Base64.RFC4648
  ( decode,
    encode,
  )
where

import Data.Array.Unboxed
import Data.Word
import qualified Misc.Base64 as Base64

decode :: String -> Either String [Word8]
encode :: Bool -> [Word8] -> String

alphabet =
  listArray
    (0, 63)
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ::
    Base64.Alphabet

padChar = Just '='

encode isPadded = Base64.encode alphabet p
  where
    p
      | isPadded = padChar
      | otherwise = Nothing

decode = Base64.decode alphabet padChar
