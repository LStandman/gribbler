-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module BNF.Text(
    match_char,
    match_text)
  where

import BNF

match_char :: Char -> Parser String String
match_char c = \ xs -> case xs of
  []     -> Miss
  (y:ys) -> case c == y of
    True  -> Hit (ys, [y])
    False -> Miss

match_text :: [Char] -> Parser String String
match_text [] = non
match_text s  = foldl1 (et) $ map (match_char) s
