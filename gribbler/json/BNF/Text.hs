-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module BNF.Text(
    module DiffList,
    TextParser,
    TextResult,
    any_char,
    match_char,
    match_text)
  where

import qualified BNF as BNF
import DiffList

type TextParser = BNF.Parser String DiffString
type TextResult = BNF.Result (DiffString, String)

any_char   :: [Char] -> TextParser
match_char :: Char -> TextParser
match_text :: [Char] -> TextParser

match_char c = BNF.Parser (\ xs -> case xs of
  []     -> BNF.Miss
  (y:ys) -> case c == y of
    True  -> BNF.Hit (difflist [c], ys)
    False -> BNF.Miss)

match_text [] = BNF.null
match_text s  = foldl1 (BNF.and) $ map (match_char) s

any_char s = foldl1 (BNF.or) $ map (match_char) s
