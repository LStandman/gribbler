-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF/Extras.hs: Extra functions for syntax analysis of text
-- Copyright (C) 2022-2023 LStandman


module BNF.Extras(match_char)
  where

import BNF

match_char :: Char -> Parser String String
match_char c []     = Miss
match_char c (x:xs) = case c == x of
  True  -> Hit xs [x]
  False -> Miss
