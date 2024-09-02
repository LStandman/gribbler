-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/MemUtils.hs: String utilities
-- Copyright (C) 2021-2024 LStandman

module Misc.MemUtils(
    hex2num,
    num2hex,
    num2hex1,
    runcons,
    strBytes)
  where

import Data.Char
import Data.List
import Data.Word
import GHC.Stack

hex2num  :: String -> Maybe Int
num2hex  :: Int -> Int -> String
num2hex1 :: Int -> String
runcons  :: HasCallStack => [a] -> (a, [a])
strBytes :: String -> [Word8]

strBytes = map (fromIntegral . fromEnum)

hexes = ['0'..'9'] ++ ['A'..'F']

hexChar c = elemIndex (toUpper c) (hexes)

hex2num [] = Nothing
hex2num s  = mapM (hexChar) s >>= Just . foldl' (\ a b -> a * 16 + b) 0

num2hex 0 n =
  case n `quotRem` 16 of
    (0, r) -> [hexes!!r]
    (q, r) -> num2hex 0 q ++ [hexes!!r]

num2hex 1 n = num2hex 0 n

num2hex p n =
  case n `quotRem` 16 of
    (q, r) -> num2hex (p - 1) q ++ [hexes!!r]

num2hex1 = num2hex 0

runcons [] = error "Misc.MemUtils.runcons: empty list"
runcons [y] = (y, [])
runcons (x:xs) = (y, x:ys)
  where
    (y, ys) = runcons xs
