-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMoo/GCM.hs: GCM mode of operation
-- Copyright (C) 2024 LStandman

module Crypt.MooMoo.GCM
  ( decrypt,
    decrypt1,
    encrypt,
    encrypt1,
  )
where

import Data.Bits
import Data.List
import Data.Word
import GHC.Stack

type BlockCipher = [Word8] -> [Word8]

decrypt1 ::
  HasCallStack => BlockCipher -> [Word8] -> Int -> [Word8] -> Int -> ([Word8], [Word8]) -> Int -> Either String [Word8]
decrypt ::
  HasCallStack => BlockCipher -> [Word8] -> [Word8] -> ([Word8], [Word8]) -> Either String [Word8]
encrypt1 ::
  HasCallStack => BlockCipher -> [Word8] -> Int -> [Word8] -> Int -> [Word8] -> Int -> ([Word8], [Word8])
encrypt ::
  HasCallStack => BlockCipher -> [Word8] -> [Word8] -> [Word8] -> ([Word8], [Word8])

gcmSizeBlock = 16

gcmSizeTag = 16

xtime :: Integer -> Integer
xtime v = (v `shiftR` 1) `xor` mask
  where
    mask
      | (v .&. 1) /= 0 = 0xE1000000000000000000000000000000
      | otherwise = 0

dot' :: Integer -> Integer -> Int -> Integer
dot' _ _ 0 = 0
dot' x y i = z `xor` dot' x (xtime y) i'
  where
    i' = i - 1
    z
      | ((x `shiftR` i') .&. 1) /= 0 = y
      | otherwise = 0

dot :: Integer -> Integer -> Integer
dot x y = dot' x y 128

blockToInt :: [Word8] -> Integer
blockToInt = foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0

intToBlock :: Integer -> [Word8]
intToBlock x =
  map
    fromIntegral
    [ x `shiftR` 120,
      x `shiftR` 112,
      x `shiftR` 104,
      x `shiftR` 96,
      x `shiftR` 88,
      x `shiftR` 80,
      x `shiftR` 72,
      x `shiftR` 64,
      x `shiftR` 56,
      x `shiftR` 48,
      x `shiftR` 40,
      x `shiftR` 32,
      x `shiftR` 24,
      x `shiftR` 16,
      x `shiftR` 8,
      x
    ] ::
    [Word8]

ghash' :: Integer -> Integer -> [Word8] -> Integer
ghash' _ y [] = y
ghash' h y x = ghash' h ((y `xor` blockToInt x1) `dot` h) x2
  where
    (x1, x2) = splitAt gcmSizeBlock x

ghash :: [Word8] -> [Word8] -> [Word8]
ghash h = intToBlock . ghash' (blockToInt h) 0

inc32 :: [Word8] -> [Word8]
inc32 x = msb ++ toList (toInt lsb + 1)
  where
    toInt = foldl' (\a b -> (a `shiftL` 8) .|. fromIntegral b) 0
    toList :: Integer -> [Word8]
    toList x =
      map
        fromIntegral
        [x `shiftR` 24, x `shiftR` 16, x `shiftR` 8, x] ::
        [Word8]
    (msb, lsb) = splitAt 12 x

gctr :: BlockCipher -> [Word8] -> [Word8] -> [Word8]
gctr f cb [] = []
gctr f cb x = zipWith xor x1 (f cb) ++ gctr f cb' x2
  where
    (x1, x2) = splitAt gcmSizeBlock x
    cb' = inc32 cb

xcrypt ::
  HasCallStack => Bool -> BlockCipher -> [Word8] -> Int -> [Word8] -> Int -> [Word8] -> Int -> ([Word8], [Word8])
xcrypt authPostText f iv ivLen auth authLen preText textLen = (postText, t)
  where
    split x =
      map
        fromIntegral
        [ x `shiftR` 56,
          x `shiftR` 48,
          x `shiftR` 40,
          x `shiftR` 32,
          x `shiftR` 24,
          x `shiftR` 16,
          x `shiftR` 8,
          x
        ] ::
        [Word8]
    n = (gcmSizeBlock - ivLen) `mod` gcmSizeBlock
    u = (gcmSizeBlock - textLen) `mod` gcmSizeBlock
    v = (gcmSizeBlock - authLen) `mod` gcmSizeBlock
    h = f $ replicate gcmSizeBlock 0
    j0
      | ivLen == 0 = error "Crypt.MooMoo.GCM.xcrypt: unsupported IV length <0>"
      | ivLen == 12 = iv ++ [0, 0, 0, 1]
      | otherwise = ghash h (iv ++ replicate (n + 8) 0 ++ split (ivLen * 8))
    postText = gctr f (inc32 j0) preText
    cc
      | authPostText = postText
      | otherwise = preText
    s =
      ghash
        h
        ( auth ++ replicate v 0 ++ cc ++ replicate u 0
            ++ split (authLen * 8)
            ++ split (textLen * 8)
        )
    t = take gcmSizeTag $ gctr f j0 s

encrypt1 = xcrypt True

encrypt f iv auth ptext =
  encrypt1 f iv (length iv) auth (length auth) ptext (length ptext)

decrypt1 f iv ivLen auth authLen (ctext, tag) textLen =
  if t == tag then Right p else Left "Authentication failed"
  where
    (p, t) = xcrypt False f iv ivLen auth authLen ctext textLen

decrypt f iv auth (ctext, tag) =
  decrypt1 f iv (length iv) auth (length auth) (ctext, tag) (length ctext)
