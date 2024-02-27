-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64/RFC4648.hs: Base64 RFC4648 implementation.
-- Copyright (C) 2024 LStandman

module Misc.Base64.RFC4648(
    encode)
  where

import Data.Array.Unboxed
import Data.Word
--
import qualified Misc.Base64 as Base64

encode :: Bool -> [Word8] -> String

alphabet =
  listArray (0, 63)
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
      :: Base64.Alphabet

encode padded v  =
  case splitAt 3 v of
    ([], []) -> ""
    (xs, []) -> (take (n + 1) $ f (xs ++ repeat 0)) ++ padding
    (xs, v') -> f xs ++ encode padded v'
  where
    n = length v
    padding
      | padded    = take (3 - n) $ repeat '='
      | otherwise = ""
    f (x1:x2:x3:_) =
      case Base64.encode alphabet (x1, x2, x3) of
        (a, b, c, d) -> [a, b, c, d]
