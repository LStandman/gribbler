-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2023 LStandman

module JSON.BNF.Text(
    module Misc.DiffList,
    TextState,
    assert_noop,
    assert_pop,
    assert_push,
    get_any_char,
    get_any_char1,
    get_char,
    get_char1,
    get_char_with,
    get_text,
    meta_break,
    meta_char,
    meta_eof,
    text_state)
  where

import qualified JSON.BNF as BNF
import Misc.DiffList

type TextState = (String, (Int, Int), [((Int, Int), String)])

assert_noop       :: String -> BNF.Parser TextState a -> BNF.Parser TextState a
assert_pop        :: String -> BNF.Parser TextState a -> BNF.Parser TextState a
assert_push       :: BNF.Parser TextState a -> BNF.Parser TextState a
get_any_char      :: [Char] -> BNF.Parser TextState Char
get_any_char1     :: [Char] -> BNF.Parser TextState DiffString
get_char          :: Char -> BNF.Parser TextState Char
get_char1         :: Char -> BNF.Parser TextState DiffString
get_char_with     :: (Char -> Bool) -> BNF.Parser TextState Char
get_text          :: [Char] -> BNF.Parser TextState DiffString
meta_break        :: Monoid a => BNF.Parser TextState a
meta_char         :: Monoid a => Char -> BNF.Parser TextState a
meta_eof          :: Monoid a => BNF.Parser TextState a
text_state        :: String -> TextState

size_trace = 10

text_state s = (s, (0, 0), [])

to_difflist :: Char -> BNF.Parser TextState DiffString
to_difflist = return . difflist . (:[])

get_symbol :: BNF.Parser TextState Char
get_symbol =
  BNF.Parser (
    \ (xs, (line, col), stack) -> case xs of
      []     -> BNF.Miss
      (y:ys) -> BNF.Hit (y, (ys, (line, col + 1), stack)))

get_char c =
  get_symbol >>=
    \ y -> case c == y of
      True  -> return c
      False -> BNF.miss


get_char_with f =
  get_symbol >>=
    \ y -> case  f y of
      True  -> return $ y
      False -> BNF.miss

get_text [] = BNF.null
get_text s  =
  foldl1 (BNF.and) $
  map (\ c -> get_char c >>= to_difflist) s

get_any_char s = foldl1 (BNF.or) $ map (get_char) s

get_char1 c = get_char c >>= to_difflist

get_any_char1 s = get_any_char s >>= to_difflist

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
        get_char '\x000A' >> get_char '\x000D' `BNF.or`
        get_char '\x000D' `BNF.or`
        get_char '\x000A')
      (s, (line, col), stack) >>=
        \ (s', _, stack') ->
          return (mempty, (s', (line + 1, 0), stack')))

meta_eof =
  BNF.Parser (
    \ (s, ctx, stack) -> case s of
      [] -> BNF.Hit (mempty, (s, ctx, stack))
      _  -> BNF.Miss)
