-- SPDX-License-Identifier: GPL-3.0-or-later
-- Diceware.hs: Encode and decode numbers to dictionary entries
-- Copyright (C) 2021 LStandman

module Diceware(
    diceware_decode,
    diceware_encode,
    diceware_is_sanitized)
  where

import Data.Char
import Data.List
import Data.Maybe

diceware_decode' :: [String] -> Int -> [String] -> Maybe (Int, Int)

diceware_decode' _ _ [] = Just (0, 1)

diceware_decode' dictionary radix (hit:hits) =
  elemIndex hit dictionary >>=
  \ x -> diceware_decode' dictionary radix hits >>=
  \ (y, position) -> Just (position * x + y, position * radix)

-- Decodes a number from list `hits` of entries from `dictionary`.
-- First entry represents the most significant digit.
diceware_decode :: [String] -> [String] -> Maybe Int

diceware_decode dictionary hits =
  diceware_decode' dictionary (length dictionary) hits >>=
  \ (x, _) -> Just x

diceware_encode' :: [String] -> Int -> Int -> Int -> [String]

diceware_encode' _ _ 0 _ = []

diceware_encode' dictionary radix digits number =
  (diceware_encode' dictionary radix (digits - 1) q) ++ [dictionary!!r]
  where
    (q, r) = number `divMod` radix

-- Encodes a number into a list of dictionary entries from `dictionary`.
-- First entry represents the most significant digit.
diceware_encode :: [String] -> Int -> Int -> [String]

diceware_encode dictionary digits number =
  diceware_encode' dictionary (length dictionary) digits number

-- To avoid undefined behavior the dictionary should be:
-- * Sorted.
-- * Unique.
-- * Only containining letters of the alphabet.
-- * And only in lower case.
diceware_is_sanitized :: [String] -> Bool

diceware_is_sanitized [] = True

diceware_is_sanitized (x:xs) =
  (isNothing $ find (not . isLower) x) &&
  (isNothing $ find (<= x) xs) &&
  diceware_is_sanitized xs
