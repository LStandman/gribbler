-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module BNF.Text(
    module DiffList,
    TextParser,
    drop_char,
    get_any_char,
    get_char,
    get_text)
  where

import qualified BNF as BNF
import DiffList
import qualified Magma as Magma

type TextParser = BNF.Parser String DiffString

get_char     :: Char -> TextParser
get_any_char :: [Char] -> TextParser
get_text     :: [Char] -> TextParser
drop_char    :: Magma.UnitalMagma a => Char -> BNF.Parser String a

get_char c = BNF.Parser (\ xs -> case xs of
  []     -> BNF.Miss
  (y:ys) -> case c == y of
    True  -> BNF.Hit (difflist [c], ys)
    False -> BNF.Miss)

get_text [] = BNF.null
get_text s  = foldl1 (BNF.and) $ map (get_char) s

get_any_char s = foldl1 (BNF.or) $ map (get_char) s

drop_char c = BNF.drop $ get_char c
