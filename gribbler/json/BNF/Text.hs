-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module BNF.Text(
    module DiffList,
    TextParser,
    TextResult,
    match_char,
    match_text)
  where

import BNF
import DiffList

type TextParser = Parser String DiffString
type TextResult = Result String DiffString

match_char :: Char -> TextParser
match_text :: [Char] -> TextParser

match_char c = \ xs -> case xs of
  []     -> Miss
  (y:ys) -> case c == y of
    True  -> Hit (ys, difflist [c])
    False -> Miss

match_text [] = nul
match_text s  = foldl1 (et) $ map (match_char) s
