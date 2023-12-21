-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module JSON.BNF.Text(
    module Misc.DiffList,
    TextParser,
    drop_char,
    get_any_char,
    get_char,
    get_char_in_range,
    get_eof,
    get_text)
  where

import qualified JSON.BNF as BNF
import Misc.DiffList

type TextParser = BNF.Parser String DiffString

drop_char         :: Monoid a => Char -> BNF.Parser String a
get_eof           :: Monoid a => BNF.Parser String a
get_any_char      :: [Char] -> TextParser
get_char          :: Char -> TextParser
get_char_in_range :: (Char, Char) -> TextParser
get_text          :: [Char] -> TextParser

get_eof = BNF.Parser (\ ys -> case ys of
  [] -> return mempty
  _  -> BNF.Miss)

get_char' :: BNF.Parser String Char
get_char' = BNF.Parser (\ xs -> case xs of
  []     -> BNF.Error "Unexpected EOF"
  (y:ys) -> BNF.Hit (y, ys))

get_char c = get_char' >>=
  \ y -> case c == y of
    True  -> return $ difflist [c]
    False -> BNF.miss

get_char_in_range (a, b) = get_char' >>=
  \ y -> case  (a <= y) && (y <= b) of
    True  -> return $ difflist [y]
    False -> BNF.miss

get_text [] = BNF.null
get_text s  = foldl1 (BNF.and) $ map (get_char) s

get_any_char s = foldl1 (BNF.or) $ map (get_char) s

drop_char c = BNF.drop $ get_char c
