-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module JSON.BNF.Text(
    module Misc.DiffList,
    CharParser,
    TextParser,
    drop_char,
    get_any_char,
    get_any_char1,
    get_char,
    get_char1,
    get_text)
  where

import qualified JSON.BNF as BNF
import Misc.DiffList

type CharParser = BNF.Parser String Char
type TextParser = BNF.Parser String DiffString

get_char      :: Char -> TextParser
get_char1     :: Char -> CharParser
get_any_char  :: [Char] -> TextParser
get_any_char1 :: [Char] -> CharParser
get_text      :: [Char] -> TextParser
drop_char     :: Monoid a => Char -> BNF.Parser String a

get_char1 c = BNF.Parser (\ xs -> case xs of
  []     -> BNF.Miss
  (y:ys) -> case c == y of
    True  -> BNF.Hit (c, ys)
    False -> BNF.Miss)

get_char c = get_char1 c >>= return . difflist . (:[])

get_text [] = BNF.null
get_text s  = foldl1 (BNF.and) $ map (get_char) s

get_any_char1 s = foldl1 (BNF.or) $ map (get_char1) s

get_any_char s = get_any_char1 s >>= return . difflist . (:[])

drop_char c = BNF.drop $ get_char c
