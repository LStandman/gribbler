-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module JSON.BNF.Text(
    module Misc.DiffList,
    TextParser,
    TextState,
    assert_noop,
    assert_pop,
    assert_push,
    get_any_char,
    get_char,
    get_char_in_range,
    get_text,
    meta_break,
    meta_char,
    text_state)
  where

import qualified JSON.BNF as BNF
import Misc.DiffList

type TextState = (String, (Int, Int), [((Int, Int), String)])
type TextParser = BNF.Parser TextState DiffString

assert_noop       :: String -> BNF.Parser TextState a -> BNF.Parser TextState a
assert_pop        :: String -> BNF.Parser TextState a -> BNF.Parser TextState a
assert_push       :: BNF.Parser TextState a -> BNF.Parser TextState a
get_any_char      :: [Char] -> TextParser
get_char          :: Char -> TextParser
get_char_in_range :: (Char, Char) -> TextParser
get_text          :: [Char] -> TextParser
meta_break        :: Monoid a => BNF.Parser TextState a
meta_char         :: Monoid a => Char -> BNF.Parser TextState a
text_state        :: String -> TextState

size_trace = 10

text_state s = (s, (0, 0), [])

get_char' :: BNF.Parser TextState Char
get_char' =
  BNF.Parser (
    \ (xs, (line, col), stack) -> case xs of
      []     -> BNF.Miss
      (y:ys) -> BNF.Hit (y, (ys, (line, col + 1), stack)))

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

meta_char c = BNF.drop $ get_char c

assert_push f =
  BNF.Parser (
    \ (s, ctx, stack) ->
      BNF.run_parser f (s, ctx, (ctx, take size_trace s):stack))

assert_pop e f =
  BNF.Parser (
    \ (s, ctx, ((line', col'), trace):stack) ->
      BNF.run_parser (
        BNF.assert
          (e ++ " :: starting at line " ++ (show line') ++ ", col " ++
          (show col') ++ ", " ++ "trace :: " ++ trace) f)
        (s, ctx, stack))

assert_noop e f =
  assert_push (BNF.null :: BNF.Parser TextState ()) >>
    assert_pop e f

-- NOTE: Adapted from YAML to account for any non-content line break.
meta_break =
  BNF.Parser (
    \  (s, (line, col), stack) -> BNF.exec_parser (
        get_char '\x000A' `BNF.and` get_char '\x000D' `BNF.or`
        get_char '\x000D' `BNF.or`
        get_char '\x000A')
      (s, (line, col), stack) >>=
        \ (s', _, stack') ->
          return (mempty, (s', (line + 1, 0), stack')))
