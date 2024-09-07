-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64.hs: Base64 implementation
-- Copyright (C) 2024 LStandman

module Misc.Base64
  ( Alphabet,
    decode,
    encode,
  )
where

import Data.Array.Unboxed
import Data.Bits
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
import Data.Word

type Alphabet = UArray Word8 Char

encode :: Alphabet -> Maybe Char -> [Word8] -> String
decode :: Alphabet -> Maybe Char -> String -> Either String [Word8]
encodeChunk' :: Alphabet -> (Word8, Word8, Word8) -> (Char, Char, Char, Char)
encodeChunk' alphabet (a, b, c) =
  ( alphabet ! (a `shiftR` 2),
    alphabet ! ((a `shiftL` 4 .|. b `shiftR` 4) .&. 0x3F),
    alphabet ! ((b `shiftL` 2 .|. c `shiftR` 6) .&. 0x3F),
    alphabet ! (c .&. 0x3F)
  )

encodeChunk :: Alphabet -> [Word8] -> [Char]
encodeChunk alphabet (a : b : c : _) =
  case encodeChunk' alphabet (a, b, c) of (x1, x2, x3, x4) -> [x1, x2, x3, x4]

encodeLast :: Alphabet -> Maybe Char -> [Word8] -> String
encodeLast _ _ [] = ""
encodeLast alphabet pad_char v =
  -- Only take the _N+1_ chars that encode _N_ bytes of data.
  -- And up to 4 chars total, including padding.
  take
    4
    ( take (n + 1) (encodeChunk alphabet (v ++ repeat 0))
        ++ maybe "" repeat pad_char
    )
  where
    n = length v

encode alphabet pad_char v =
  case splitAt 3 v of
    (v1, []) -> encodeLast alphabet pad_char v1
    (v1, v2) ->
      encodeChunk alphabet v1 ++ encode alphabet pad_char v2

alphaIndex :: [Char] -> Char -> Either String Word8
alphaIndex alphabet' c =
  case elemIndex c alphabet' of
    Just i -> Right . fromIntegral $ i
    Nothing -> Left ("Encoding character is not in alphabet " ++ show c)

decodeChunk' :: [Char] -> (Char, Char, Char, Char) -> Either String (Word8, Word8, Word8)
decodeChunk' alphabet' (x1, x2, x3, x4) =
  alphaIndex alphabet' x1
    >>= \i1 ->
      alphaIndex alphabet' x2
        >>= \i2 ->
          alphaIndex alphabet' x3
            >>= \i3 ->
              alphaIndex alphabet' x4
                >>= \i4 ->
                  return
                    ( i1 `shiftL` 2 .|. i2 `shiftR` 4,
                      i2 `shiftL` 4 .|. i3 `shiftR` 2,
                      i3 `shiftL` 6 .|. i4
                    )

decodeChunk :: [Char] -> String -> Either String [Word8]
decodeChunk alphabet' (x1 : x2 : x3 : x4 : _) =
  (\(a, b, c) -> [a, b, c]) <$> decodeChunk' alphabet' (x1, x2, x3, x4)

decodeLast' :: [Char] -> Char -> Maybe Char -> String -> Either String [Word8]
decodeLast' _ _ _ [] = Right []
decodeLast' alphabet' zero_char pad_char v
  | n >= 2 =
    decodeChunk alphabet' (v' ++ repeat zero_char)
      <&> take (n - 1)
  | otherwise = Left ("Bad last chunk length after discarding pad. " ++ show n)
  where
    v' = maybe v (\c -> dropWhileEnd (c ==) v) pad_char
    n = length v'

decode' :: [Char] -> Char -> Maybe Char -> String -> Either String [Word8]
decode' alphabet' zero_char pad_char v =
  case splitAt 4 v of
    (v1, []) -> decodeLast' alphabet' zero_char pad_char v1
    (v1, v2) ->
      decodeChunk alphabet' v1
        >>= \xs ->
          decode' alphabet' zero_char pad_char v2
            >>= \ys -> return (xs ++ ys)

decode alphabet = decode' (elems alphabet) (alphabet ! 0)
