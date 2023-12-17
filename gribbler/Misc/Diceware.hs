-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Diceware.hs: Encode and decode numbers to dictionary entries
-- Copyright (C) 2021-2023 LStandman

module Misc.Diceware(
    decode,
    encode,
    is_sanitized)
  where

import Data.Char
import Data.List
import Data.Maybe
--
import Misc.DiffList
import Misc.MemUtils
  
-- Decodes a number from list `hits` of entries from `dictionary`.
-- First entry represents the most significant digit.
decode :: [String] -> [String] -> Maybe Int

-- Encodes a number into a list of dictionary entries from `dictionary`.
-- First entry represents the most significant digit.
encode :: [String] -> Int -> Int -> [String]

-- To avoid undefined behavior the dictionary should be:
-- * Sorted.
-- * Unique.
-- * Only containining letters of the alphabet.
-- * And only in lower case.
is_sanitized :: [String] -> Bool

decode dictionary hits =
  maybeMap (flip elemIndex dictionary) hits >>=
  Just . (foldl (\ a b -> a * (length dictionary) + b) 0)

encode' :: [String] -> Int -> Int -> Int -> DiffList String
encode' _ _ 0 _ = difflist []
encode' dictionary radix digits number =
  (encode' dictionary radix (digits - 1) q) <> difflist [dictionary!!r]
  where
    (q, r) = number `divMod` radix

encode dictionary digits number =
  relist $ 
  encode' dictionary (length dictionary) digits number

is_sanitized' :: String -> Bool
is_sanitized' = isNothing . (find (not . isLower))

is_sanitized [] = True
is_sanitized (x:xs) =
  is_sanitized' x &&
  (isNothing $ find (<= x) xs) &&
  is_sanitized xs
