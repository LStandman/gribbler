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
import Data.Maybe (maybe)
import Data.Word

type Alphabet = UArray Word8 Char

encode :: Alphabet -> Maybe Char -> [Word8] -> String
decode :: Alphabet -> Maybe Char -> String -> Either String [Word8]
encodeChunk :: Alphabet -> (Word8, Word8, Word8) -> (Char, Char, Char, Char)
encodeChunk alphabet (a, b, c) =
  ( alphabet ! (a `shiftR` 2),
    alphabet ! ((a `shiftL` 4 .|. b `shiftR` 4) .&. 0x3F),
    alphabet ! ((b `shiftL` 2 .|. c `shiftR` 6) .&. 0x3F),
    alphabet ! (c .&. 0x3F)
  )
encode _ _ [] = []
encode alphabet padChar (a : b : c : v) =
  case encodeChunk alphabet (a, b, c) of
    (x1, x2, x3, x4) -> [x1, x2, x3, x4] ++ encode alphabet padChar v
encode alphabet padChar v =
  -- Only take the _N+1_ chars that encode _N_ bytes of data.
  -- And up to 4 chars total, including padding.
  take (n + 1) (encode alphabet padChar (v ++ replicate padN 0))
    ++ maybe "" (replicate padN) padChar
  where
    n = length v
    padN = 3 - n

alphaIndex :: [Char] -> Char -> Either String Word8
alphaIndex alphabet' c =
  case elemIndex c alphabet' of
    Just i -> Right . fromIntegral $ i
    Nothing -> Left ("Encoding character is not in alphabet " ++ show c)

decodeChunk ::
  [Char] ->
  (Char, Char, Char, Char) ->
  Either String (Word8, Word8, Word8)
decodeChunk alphabet' (x1, x2, x3, x4) =
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

decode' :: [Char] -> Maybe Char -> String -> Either String [Word8]
decode' _ _ [] = Right []
decode' alphabet' (Just padChar) [x1, x2, x3, x4]
  | n >= 2 = decode' alphabet' Nothing v'
  | otherwise = Left ("Bad last chunk length after discarding pad. " ++ show n)
  where
    v' = dropWhileEnd (padChar ==) [x1, x2, x3, x4]
    n = length v'
decode' alphabet' padChar (x1 : x2 : x3 : x4 : v) =
  liftA2
    (++)
    ((\(a, b, c) -> [a, b, c]) <$> decodeChunk alphabet' (x1, x2, x3, x4))
    (decode' alphabet' padChar v)
decode' alphabet' padChar v =
  decode' alphabet' padChar (v ++ replicate (4 - n) (head alphabet'))
    <&> take (n - 1)
  where
    n = length v

decode alphabet = decode' (elems alphabet)
