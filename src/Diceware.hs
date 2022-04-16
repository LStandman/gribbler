-- SPDX-License-Identifier: GPL-3.0-or-later
-- Diceware.hs: Encode and decode with a Diceware dictionary
-- Copyright (C) 2021 LStandman

module Diceware(
    diceware_encode)
  where

import Data.List

diceware_encode' :: [String] -> Int -> Int -> [String] -> Maybe Int

diceware_encode' _ _ _ [] = Just 0

diceware_encode' dictionary radix position hits =
  elemIndex hit dictionary >>=
  \ x -> diceware_encode' dictionary radix (position * radix) hits' >>=
  \ y -> Just (position * x + y)
  where
    hit:hits' = hits

diceware_encode :: [String] -> [String] -> Maybe Int

diceware_encode [] _ = Nothing

diceware_encode _ [] = Nothing

diceware_encode dictionary hits =
  diceware_encode' dictionary (length dictionary) 1 hits
