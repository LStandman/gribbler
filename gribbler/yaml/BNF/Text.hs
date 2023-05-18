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

type Text = (String, String)

conv        :: Parser Text -> (String -> String) -> Parser Text
match_char  :: Char -> Parser Text
text :: String -> Text

--format  :: (String -> String) -> Text -> Text
--format f (a, b) = (a, f b)

--combiner :: Parser Text -> Parser Text
--combiner f = \ (s, p) -> fmap (format (p ++)) $ f (s, p)

--to_parser :: (String -> Result Text) -> Parser Text
--to_parser f = \ (s, p) -> f s

conv'' :: (String -> String) -> Text -> Text
conv'' f (s, p) = (s, f p)

conv' :: (String -> String) -> Result Text -> Result Text
conv' f r = fmap (conv'' f) r

conv f g = \ (s, p) -> (conv' ((p ++) . g) $ f (s, p))

text s = (s, "")

match_char c ([],   _)   = Miss
match_char c (x:xs, p) = case c == x of
  True  -> Hit (xs, p ++ [x])
  False -> Miss
