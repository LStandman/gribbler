-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/MemUtils.hs: String utilities
-- Copyright (C) 2021-2023 LStandman

module Misc.MemUtils(
    hex2num,
    strBytes)
  where

import Data.Char
import Data.List
import Data.Word

hex2num  :: String -> Maybe Int
strBytes :: String -> [Word8]

strBytes = map (fromIntegral . fromEnum)

hexChar c = elemIndex (toUpper c) (['0'..'9'] ++ ['A'..'F'])

hex2num  []     = Nothing
hex2num  s      = mapM (hexChar) s >>= Just . foldl (\ a b -> a * 16 + b) 0
