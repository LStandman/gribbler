-- SPDX-License-Identifier: GPL-3.0-or-later
-- MemUtils.hs: String utilities
-- Copyright (C) 2023 LStandman

module MemUtils(
    hex2num,
    strBytes,
    maybeMap)
  where

import Data.Char
import Data.List
import Data.Word

hex2num  :: String -> Maybe Int
strBytes :: String -> [Word8]
maybeMap :: (a -> Maybe b) -> [a] -> Maybe [b]

strBytes = map (fromIntegral . fromEnum)

hexChar c = elemIndex (toUpper c) (['0'..'9'] ++ ['A'..'F'])

hex2num  []     = Nothing
hex2num  s      = maybeMap (hexChar) s >>= Just . foldl (\ a b -> a * 16 + b) 0

maybeMap _ [] = Just []
maybeMap f (x:xs) =
  f x >>= \ y -> maybeMap f xs >>= \ ys -> Just $ y:ys
