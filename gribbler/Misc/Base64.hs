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

encode :: Alphabet -> Maybe Char -> [Word8] -> String
decode :: Alphabet -> Maybe Char -> String -> Either String [Word8]

encode_chunk' :: Alphabet -> (Word8, Word8, Word8) -> (Char, Char, Char, Char)
encode_chunk' alphabet (a, b, c) =
  ( alphabet!(a `shiftR` 2),
    alphabet!((a `shiftL` 4 .|. b `shiftR` 4) .&. 0x3F),
    alphabet!((b `shiftL` 2 .|. c `shiftR` 6) .&. 0x3F),
    alphabet!(c .&. 0x3F))

encode_chunk :: Alphabet -> [Word8] -> [Char]
encode_chunk alphabet (a:b:c:_) =
  case encode_chunk' alphabet (a, b, c) of (x1, x2, x3, x4) -> [x1, x2, x3, x4]

encode_last :: Alphabet -> Maybe Char -> [Word8] -> String
encode_last alphabet pad_char v =
  -- Only take the _N+1_ chars that encode _N_ bytes of data.
  -- And up to 4 chars total, including padding.
  take 4 $ (take (n + 1) $ encode_chunk alphabet (v ++ repeat 0)) ++ padding
  where
    n       = length v
    padding =
      case pad_char of
        Just p  -> repeat p
        Nothing -> ""

encode alphabet pad_char v =
  case splitAt 3 v of
    ([], []) -> ""
    (u,  []) ->
      encode_last alphabet pad_char u
    (u,  v') ->
      encode_chunk alphabet u ++ encode alphabet pad_char v'

alphaIndex :: [Char] -> Char -> Either String Word8
alphaIndex alphabet' c =
  case elemIndex c alphabet' of
    Just i  -> Right . fromIntegral $ i
    Nothing -> Left $ "Character not in alphabet (or out of place padding)" ++ show c

decode_chunk' :: [Char] -> (Char, Char, Char, Char) -> Either String (Word8, Word8, Word8)
decode_chunk' alphabet' (x1, x2, x3, x4) =
  alphaIndex alphabet' x1 >>= \ i1 ->
    alphaIndex alphabet' x2 >>= \ i2 ->
      alphaIndex alphabet' x3 >>= \ i3 ->
        alphaIndex alphabet' x4 >>= \ i4 ->
          return
            ( i1 `shiftL` 2 .|. i2 `shiftR` 4,
              i2 `shiftL` 4 .|. i3 `shiftR` 2,
              i3 `shiftL` 6 .|. i4)

decode_chunk :: [Char] -> String -> Either String [Word8]
decode_chunk alphabet' (x1:x2:x3:x4:_) =
  fmap (\ (a, b, c) -> [a, b, c]) $ decode_chunk' alphabet' (x1, x2, x3, x4)

decode_last' :: [Char] -> Char -> Maybe Char -> String -> Either String [Word8]
decode_last' alphabet' zero_char pad_char v =
  decode_chunk alphabet' (v' ++ repeat zero_char) >>= return . (take (n - 1))
  where
    v' = case pad_char of
           Just p  -> dropWhileEnd (p ==) v
           Nothing -> v
    n  = length v'

decode' :: [Char] -> Char -> Maybe Char -> String -> Either String [Word8]
decode' alphabet' zero_char pad_char v =
  case splitAt 4 v of
    ([], []) -> Right []
    (u,  []) -> decode_last' alphabet' zero_char pad_char u
    (u,  v') ->
      decode_chunk alphabet' u >>= \ xs ->
        decode' alphabet' zero_char pad_char v' >>= \ ys ->
          return (xs ++ ys)

decode alphabet pad_char v = decode' (elems alphabet) (alphabet!0) pad_char v
