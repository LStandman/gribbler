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
--
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
decodeUcoord u = decodeLe (u_lsbs ++ [u_msb .&. 0x7F])
  where
    (u_msb, u_lsbs) = MemUtils.runcons u

encodeUcoord :: Integer -> [Word8]
encodeUcoord u =
  snd $ mapAccumL (\a _ -> (a `shiftR` 8, fromIntegral a)) u' [1 .. 32]
  where
    u' = u `mod` constP

decodeScalar :: HasCallStack => [Word8] -> Integer
decodeScalar (k_lsb : ks) =
  decodeLe ((k_lsb .&. 0xF8) : (ks' ++ [(k_msb .&. 0x7F) .|. 0x40]))
  where
    (k_msb, ks') = MemUtils.runcons ks

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
cswap swap (x_2, x_3) = (x_2', x_3')
  where
    mask
      | swap =
        0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      | otherwise = 0
    dummy = mask .&. (x_2 `xor` x_3)
    x_2' = x_2 `xor` dummy
    x_3' = x_3 `xor` dummy

type Mont = (Int, Integer, Integer, Integer, Integer, Bool)

montgomery :: Integer -> Integer -> Mont -> Mont
montgomery _ _ (0, x_2, z_2, x_3, z_3, swap) =
  (0, x_2, z_2, x_3, z_3, swap)
montgomery x_1 k (t, x_2, z_2, x_3, z_3, swap) =
  montgomery
    x_1
    k
    ( t',
      aa `dot` bb,
      e `dot` (aa `add` a24 `dot` e),
      sqr (da `add` cb),
      x_1 `dot` sqr (da `sub` cb),
      k_t
    )
  where
    a24 = 121665
    t' = t - 1
    k_t = ((k `shiftR` t') .&. 1) /= 0
    swap' = swap `xor` k_t
    (x_2', x_3') = cswap swap' (x_2, x_3)
    (z_2', z_3') = cswap swap' (z_2, z_3)
    a = x_2' `add` z_2'
    aa = sqr a
    b = x_2' `sub` z_2'
    bb = sqr b
    e = aa `sub` bb
    c = x_3' `add` z_3'
    d = x_3' `sub` z_3'
    da = d `dot` a
    cb = c `dot` b

x25519 :: HasCallStack => [Word8] -> [Word8] -> [Word8]
x25519 k u = encodeUcoord (x_2' `dot` (z_2' `pow` (constP - 2)))
  where
    u' = decodeUcoord u
    k' = decodeScalar k
    (_, x_2, z_2, x_3, z_3, swap) =
      montgomery u' k' (255, 1, 0, u', 1, False)
    (x_2', x_3') = cswap swap (x_2, x_3)
    (z_2', z_3') = cswap swap (z_2, z_3)