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
  
-- Decodes a number from list `hits` of entries from `dictionary`.
-- First entry represents the most significant digit.
diceware_decode :: [String] -> [String] -> Maybe Int

-- Encodes a number into a list of dictionary entries from `dictionary`.
-- First entry represents the most significant digit.
diceware_encode :: [String] -> Int -> Int -> [String]

-- To avoid undefined behavior the dictionary should be:
-- * Sorted.
-- * Unique.
-- * Only containining letters of the alphabet.
-- * And only in lower case.
diceware_is_sanitized :: [String] -> Bool

maybeMap :: (a -> Maybe b) -> [a] -> Maybe [b]

maybeMap _ [] = Just []

maybeMap f (x:xs) =
  maybe Nothing (\ y -> maybeMap f xs >>= \ ys -> Just $ y:ys) (f x)

diceware_decode dictionary hits =
  maybeMap (\ x -> elemIndex x dictionary) hits >>=
  Just . (foldl (\ a b -> a * (length dictionary) + b) 0)

diceware_encode' :: [String] -> Int -> Int -> Int -> [String]

diceware_encode' _ _ 0 _ = []

diceware_encode' dictionary radix digits number =
  (diceware_encode' dictionary radix (digits - 1) q) ++ [dictionary!!r]
  where
    (q, r) = number `divMod` radix

diceware_encode dictionary digits number =
  diceware_encode' dictionary (length dictionary) digits number

diceware_is_sanitized' :: String -> Bool

diceware_is_sanitized' = isNothing . (find (not . isLower))

diceware_is_sanitized [] = True

diceware_is_sanitized (x:xs) =
  diceware_is_sanitized' x &&
  (isNothing $ find (<= x) xs) &&
  diceware_is_sanitized xs
