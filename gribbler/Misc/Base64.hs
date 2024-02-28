-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64.hs: Base64 implementation
-- Copyright (C) 2024 LStandman

module Misc.Base64(
    Alphabet,
    decode,
    encode)
  where

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word

type Alphabet = UArray (Word8) Char

encode :: Alphabet -> Maybe Char -> Bool -> [Word8] -> String
decode :: Alphabet -> Maybe Char -> String -> Either String [Word8]

encode_chunk :: Alphabet -> [Word8] -> String
encode_chunk alphabet (x:y:z:_) =
  [ alphabet!(x `shiftR` 2),
    alphabet!((x `shiftL` 4 .|. y `shiftR` 4) .&. 0x3F),
    alphabet!((y `shiftL` 2 .|. z `shiftR` 6) .&. 0x3F),
    alphabet!(z .&. 0x3F)]

encode_last :: Alphabet -> Maybe Char -> Bool -> [Word8] -> String
encode_last alphabet pad_char is_padded v =
  (take (n + 1) $ encode_chunk alphabet (v ++ repeat 0)) ++ padding
  where
    n = length v
    padding
      | is_padded = take (3 - n) $ repeat . fromJust $ pad_char
      | otherwise = ""

encode alphabet pad_char is_padded v =
  case splitAt 3 v of
    ([], []) -> ""
    (xs, []) ->
      encode_last alphabet pad_char is_padded xs
    (xs, v') ->
      encode_chunk alphabet xs ++ encode alphabet pad_char is_padded v'

alphaIndex :: [Char] -> Char -> Either String Word8
alphaIndex alphabet' c =
  case elemIndex c alphabet' of
    Just i  -> Right . fromIntegral $ i
    Nothing -> Left $ "Character not in alphabet" ++ show c

decode_chunk :: [Char] -> String -> Either String [Word8]
decode_chunk alphabet' (a:b:c:d:_) =
  alphaIndex alphabet' a >>= \ i0 ->
    alphaIndex alphabet' b >>= \ i1 ->
      alphaIndex alphabet' c >>= \ i2 ->
        alphaIndex alphabet' d >>= \ i3 ->
          return
            [ i0 `shiftL` 2 .|. i1 `shiftR` 4,
              i1 `shiftL` 4 .|. i2 `shiftR` 2,
              i2 `shiftL` 6 .|. i3]

decode_last' :: [Char] -> Char -> Maybe Char -> String -> Either String [Word8]
decode_last' alphabet' zero_char pad_char v =
  decode_chunk alphabet' (v ++ repeat zero_char) >>= return . (take (n - 1))
  where
    n = length v

decode' :: [Char] -> Char -> Maybe Char -> String -> Either String [Word8]
decode' alphabet' zero_char pad_char v =
  case splitAt 4 v of
    ([], []) -> Right []
    (as, []) -> decode_last' alphabet' zero_char pad_char (f as)
    (as, v') ->
      decode_chunk alphabet' as >>= \ xs ->
        decode' alphabet' zero_char pad_char v' >>= \ ys ->
          return (xs ++ ys)
  where
    f = dropWhileEnd (\ c -> maybe False (c ==) pad_char)

decode alphabet pad_char v = decode' (elems alphabet) (alphabet!0) pad_char v
