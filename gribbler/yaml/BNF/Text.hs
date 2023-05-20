-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman


module BNF.Text(
    Text(..),
    conv,
    match_char,
    text)
  where

import BNF

infixl 1 `conv`

type Ctx a b = (a, b)
type Text = (String, String)

reproduce :: (b -> b) -> Result (Ctx a b) -> Result (Ctx a b)
reproduce f r = fmap (fmap f) r

conv :: Semigroup b => Parser (Ctx a b) -> (b -> b) -> Parser (Ctx a b)
conv f g = \ (i, o) -> reproduce ((o <>) . g) $ f (i, o)

match_char  :: Char -> Parser Text
text :: String -> Text

text s = (s, "")

match_char c ([],   _)   = Miss
match_char c (x:xs, p) = case c == x of
  True  -> Hit (xs, p ++ [x])
  False -> Miss
