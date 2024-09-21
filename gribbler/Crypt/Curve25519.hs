-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/Curve25519.hs: ECDH with X25519.
-- Copyright (C) 2024 LStandman

module Crypt.Curve25519
  ( decodeScalar,
    decodeUcoord,
    encodeUcoord,
    x25519,
  )
where

import Data.Bits
import Data.Foldable (foldr')
import Data.List
import Data.Word
import GHC.Stack
import qualified Misc.MemUtils as MemUtils (runcons)

infixl 6 `add`

infixl 6 `sub`

infixl 7 `dot`

infixr 8 `pow`

constP :: Integer
constP = 2 ^ 255 - 19

decodeLe :: [Word8] -> Integer
decodeLe = foldr' (\y x -> (x `shiftL` 8) .|. fromIntegral y) 0

decodeUcoord :: HasCallStack => [Word8] -> Integer
decodeUcoord u = decodeLe (uLsbs ++ [uMsb .&. 0x7F])
  where
    (uMsb, uLsbs) = MemUtils.runcons u

encodeUcoord :: Integer -> [Word8]
encodeUcoord u =
  snd $ mapAccumL (\a _ -> (a `shiftR` 8, fromIntegral a)) u' [1 .. 32]
  where
    u' = u `mod` constP

decodeScalar :: HasCallStack => [Word8] -> Integer
decodeScalar (kLsb : ks) =
  decodeLe ((kLsb .&. 0xF8) : (ks' ++ [(kMsb .&. 0x7F) .|. 0x40]))
  where
    (kMsb, ks') = MemUtils.runcons ks

add :: Integer -> Integer -> Integer
a `add` b = (a + b) `mod` constP

sub :: Integer -> Integer -> Integer
a `sub` b = (a - b) `mod` constP

dot :: Integer -> Integer -> Integer
a `dot` b = (a * b) `mod` constP

sqr :: Integer -> Integer
sqr a = a `dot` a

pow :: Integer -> Integer -> Integer
a `pow` 0 = 1
a `pow` 1 = a
a `pow` b = (sqr a `pow` (b `shiftR` 1)) `dot` (a `pow` (b .&. 1))

cswap :: Bool -> (Integer, Integer) -> (Integer, Integer)
cswap swap (x2, x3) = (x2', x3')
  where
    mask
      | swap =
        0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      | otherwise = 0
    dummy = mask .&. (x2 `xor` x3)
    x2' = x2 `xor` dummy
    x3' = x3 `xor` dummy

type Mont = (Int, Integer, Integer, Integer, Integer, Bool)

montgomery :: Integer -> Integer -> Mont -> Mont
montgomery _ _ (0, x2, z2, x3, z3, swap) =
  (0, x2, z2, x3, z3, swap)
montgomery x1 k (t, x2, z2, x3, z3, swap) =
  montgomery
    x1
    k
    ( t',
      aa `dot` bb,
      e `dot` (aa `add` a24 `dot` e),
      sqr (da `add` cb),
      x1 `dot` sqr (da `sub` cb),
      kt
    )
  where
    a24 = 121665
    t' = t - 1
    kt = ((k `shiftR` t') .&. 1) /= 0
    swap' = swap `xor` kt
    (x2', x3') = cswap swap' (x2, x3)
    (z2', z3') = cswap swap' (z2, z3)
    a = x2' `add` z2'
    aa = sqr a
    b = x2' `sub` z2'
    bb = sqr b
    e = aa `sub` bb
    c = x3' `add` z3'
    d = x3' `sub` z3'
    da = d `dot` a
    cb = c `dot` b

x25519 :: HasCallStack => [Word8] -> [Word8] -> [Word8]
x25519 k u = encodeUcoord (x2' `dot` (z2' `pow` (constP - 2)))
  where
    u' = decodeUcoord u
    k' = decodeScalar k
    (_, x2, z2, x3, z3, swap) =
      montgomery u' k' (255, 1, 0, u', 1, False)
    (x2', x3') = cswap swap (x2, x3)
    (z2', z3') = cswap swap (z2, z3)
