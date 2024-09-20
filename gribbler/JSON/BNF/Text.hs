-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF/Text.hs: Functions for BNF syntax analysis of text
-- Copyright (C) 2022-2024 LStandman

module JSON.BNF.Text
  ( module Misc.DiffList,
    TextState,
    assertNoop,
    assertPop,
    assertPush,
    getAnyChar,
    getAnyChar1,
    JSON.BNF.Text.getChar,
    getChar1,
    getCharWith,
    getText,
    metaBreak,
    metaChar,
    metaEof,
    textState,
  )
where

import Control.Monad (void)
import Data.Functor ((<&>))
import qualified JSON.BNF as BNF
import Misc.DiffList

type TextState = (String, (Int, Int), [((Int, Int), String)])

assertNoop :: String -> BNF.Parser TextState a -> BNF.Parser TextState a
assertPop :: String -> BNF.Parser TextState a -> BNF.Parser TextState a
assertPush :: BNF.Parser TextState a -> BNF.Parser TextState a
getAnyChar :: [Char] -> BNF.Parser TextState Char
getAnyChar1 :: [Char] -> BNF.Parser TextState DiffString
getChar :: Char -> BNF.Parser TextState Char
getChar1 :: Char -> BNF.Parser TextState DiffString
getCharWith :: (Char -> Bool) -> BNF.Parser TextState Char
getText :: [Char] -> BNF.Parser TextState DiffString
metaBreak :: BNF.Parser TextState ()
metaChar :: Char -> BNF.Parser TextState ()
metaEof :: BNF.Parser TextState ()
textState :: String -> TextState

sizeTrace = 10

textState s = (s, (0, 0), [])

toDifflist :: Char -> BNF.Parser TextState DiffString
toDifflist = return . difflist . (: [])

getSymbol :: BNF.Parser TextState Char
getSymbol =
  BNF.Parser $
    \(xs, (line, col), stack) ->
      case xs of
        [] -> BNF.Miss
        (y : ys) -> BNF.Hit (y, (ys, (line, col + 1), stack))

getChar c =
  getSymbol
    >>= \y -> (if c == y then return c else BNF.miss)

getCharWith f =
  getSymbol
    >>= \y -> (if f y then return y else BNF.miss)

stripPrefix' :: Eq a => [a] -> [a] -> Maybe ([a], Int)
stripPrefix' [] ys = Just (ys, 0)
stripPrefix' (x : xs) (y : ys)
  | x == y = stripPrefix' xs ys <&> fmap (1 +)
stripPrefix' _ _ = Nothing

-- Optimal than the naive implementation `foldl1 (BNF.and) $ map (getChar) s`
getText [] = BNF.null
getText s =
  BNF.Parser $
    \(xs, (line, col), stack) ->
      case stripPrefix' s xs of
        Nothing -> BNF.Miss
        Just (ys, n) -> BNF.Hit (difflist s, (ys, (line, col + n), stack))

-- Optimal than the naive implementation `foldl1 (BNF.or) $ map (getChar) s`
getAnyChar s =
  getSymbol
    >>= \y -> (if y `elem` s then return y else BNF.miss)

getChar1 c = JSON.BNF.Text.getChar c >>= toDifflist

getAnyChar1 s = getAnyChar s >>= toDifflist

metaChar c = void (JSON.BNF.Text.getChar c)

assertPush' :: BNF.Parser TextState ()
assertPush' =
  BNF.Parser $
    \(s, ctx, stack) ->
      BNF.Hit ((), (s, ctx, (ctx, take sizeTrace s) : stack))

assertPush f = assertPush' >> f

assertPop e f =
  BNF.Parser $
    \(s, ctx, ((line', col'), trace) : stack) ->
      ( BNF.runParser $
          BNF.assert
            ( "*** " ++ e ++ "\n  caught at " ++ show (line' + 1) ++ ":"
                ++ show (col' + 1)
                ++ ", "
                ++ "trace: "
                ++ show trace
            )
            f
      )
        (s, ctx, stack)

assertNoop e f = assertPush' >> assertPop e f

-- NOTE: Adapted from YAML to account for any non-content line break.
metaBreak =
  BNF.Parser $
    \(s, (line, col), stack) ->
      BNF.execParser
        ( metaChar '\x000A'
            `BNF.or` metaChar '\x000D' >> BNF.zoo (metaChar '\x000A')
        )
        (s, (line, col), stack)
        >>= \(s', _, stack') ->
          return ((), (s', (line + 1, 0), stack'))

metaEof =
  BNF.Parser $
    \(s, ctx, stack) ->
      case s of
        [] -> BNF.Hit ((), (s, ctx, stack))
        _ -> BNF.Miss
