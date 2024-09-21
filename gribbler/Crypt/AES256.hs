-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/AES256.hs: AES256 module
-- Copyright (C) 2021-2024 LStandman

module Crypt.AES256
  ( sizeBlock,
    sizeKey,
    encrypt,
    decrypt,
  )
where

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import GHC.Stack
import qualified Misc.MemUtils as MemUtils (runcons)

infixl 7 `dot`

type Mat = UArray (Word8, Word8) Word8

sizeBlock :: Int
sizeKey :: Int
encrypt :: HasCallStack => [Word8] -> [Word8] -> [Word8]
decrypt :: HasCallStack => [Word8] -> [Word8] -> [Word8]

boundsSbox = ((0, 0), (15, 15))

boundsSide = (0, 3)

boundsState = ((0, 0), (3, 3))

sizeBlock = 16

sizeKey = 32

sizeSide = 4

sizeState = 16

-- PRELIMINARY FUNCTIONS

xtime :: Word8 -> Word8
xtime a = (a `shiftL` 1) `xor` mask
  where
    mask
      | (a .&. 0x80) /= 0 = 0x1B
      | otherwise = 0

dot' :: Int -> Word8 -> Word8 -> Word8
dot' 0 _ 0 = 0
dot' i a b = a' `xor` dot' (i - 1) (xtime a) (b `shiftR` 1)
  where
    a'
      | (b .&. 1) /= 0 = a
      | otherwise = 0

dot :: Word8 -> Word8 -> Word8
dot = dot' 8

sub' :: Mat -> Word8 -> Word8
sub' vv a = vv ! (a `shiftR` 4, a .&. 0x0F)

fromList :: [Word8] -> Mat
fromList v =
  array boundsState (zip [(j, i) | (i, j) <- range boundsState] v)

fromCols :: [[Word8]] -> Mat
fromCols = fromList . concat

toList :: Mat -> [Word8]
toList = concat . toCols

toCols :: Mat -> [[Word8]]
toCols mat = [[mat ! (i, j) | i <- range boundsSide] | j <- range boundsSide]

-- CIPHER OPERATIONS

{- ORMOLU_DISABLE -}
sub :: Word8 -> Word8
sub = sub' sbox
  where
    -- Fig. 7.
    sbox =
      listArray
        boundsSbox
        [ 0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5,
          0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76,
          0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0,
          0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0,
          0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC,
          0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15,
          0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A,
          0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75,
          0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0,
          0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84,
          0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B,
          0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF,
          0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85,
          0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8,
          0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5,
          0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2,
          0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17,
          0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73,
          0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88,
          0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB,
          0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C,
          0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79,
          0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9,
          0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08,
          0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6,
          0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A,
          0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E,
          0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E,
          0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94,
          0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF,
          0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68,
          0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16
        ] ::
        Mat
{- ORMOLU_ENABLE -}

subBytes :: Mat -> Mat
subBytes = amap sub

shiftRows :: Mat -> Mat
shiftRows = ixmap boundsState f
  where
    f (i, j) = (i, (j + i) `mod` sizeSide)

mixColumns' :: (Word8 -> Word8 -> Word8 -> Word8 -> Word8) -> Mat -> Mat
mixColumns' f vv =
  array
    boundsState
    [ ( (i, j),
        f
          (vv ! (i, j))
          (vv ! ((i + 1) `mod` sizeSide, j))
          (vv ! ((i + 2) `mod` sizeSide, j))
          (vv ! ((i + 3) `mod` sizeSide, j))
      )
      | (i, j) <- range boundsState
    ]

mixColumns :: Mat -> Mat
mixColumns = mixColumns' mixByte
  where
    mixByte a b c d = (0x02 `dot` a) `xor` (0x03 `dot` b) `xor` c `xor` d

-- INVERSE CIPHER OPERATIONS

{- ORMOLU_DISABLE -}
invSubBytes :: Mat -> Mat
invSubBytes = amap (sub' invSbox)
  where
    -- Fig. 14.
    invSbox =
      listArray
        boundsSbox
        [ 0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38,
          0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB,
          0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87,
          0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB,
          0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D,
          0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E,
          0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2,
          0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25,
          0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16,
          0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92,
          0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA,
          0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84,
          0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A,
          0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06,
          0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02,
          0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B,
          0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA,
          0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73,
          0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85,
          0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E,
          0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89,
          0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B,
          0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20,
          0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4,
          0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31,
          0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F,
          0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D,
          0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF,
          0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0,
          0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61,
          0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26,
          0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D
        ] ::
        Mat
{- ORMOLU_ENABLE -}

invShiftRows :: Mat -> Mat
invShiftRows = ixmap boundsState f
  where
    f (i, j) = (i, (j + sizeSide - i) `mod` sizeSide)

invMixColumns :: Mat -> Mat
invMixColumns = mixColumns' mixByte
  where
    mixByte a b c d =
      (0x0E `dot` a) `xor` (0x0B `dot` b)
        `xor` (0x0D `dot` c)
        `xor` (0x09 `dot` d)

-- KEY SCHEDULE ALGORITHM

addRoundKey :: Mat -> Mat -> Mat
addRoundKey vv uu =
  listArray boundsState $ zipWith xor (elems vv) (elems uu)

keyExpansion'' :: Mat -> [Word8] -> Mat
keyExpansion'' key1 col2 =
  fromCols . tail $ scanl (zipWith xor) col2 $ toCols key1

keyExpansion' :: (Mat, Mat) -> Word8 -> (Mat, Mat)
keyExpansion' (key1, key2) rcon = (key3, key4)
  where
    -- Even rounds.
    rotWord : rotWords =
      [ sub $ key2 ! ((i + 1) `mod` sizeSide, sizeSide - 1)
        | i <- range boundsSide
      ]
    col2 = rotWord `xor` rcon : rotWords
    key3 = keyExpansion'' key1 col2
    -- Odd rounds.
    col3 =
      [ sub $ key3 ! (i, sizeSide - 1)
        | i <- range boundsSide
      ]
    key4 = keyExpansion'' key2 col3

-- this recursion on key pairs inevitably produces one key too many,
-- so we drop it.
keyExpansion :: (Mat, Mat) -> [Mat]
keyExpansion key = init $ concatMap (\(a, b) -> [a, b]) schedule
  where
    schedule =
      scanl keyExpansion' key [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40]

-- CIPHER ALGORITHM

-- `tail` drops a duplicate of key1 from schedule.y
scheduleHelper :: HasCallStack => (Mat, Mat) -> (Mat, [Mat])
scheduleHelper = MemUtils.runcons . tail . keyExpansion

cipher :: HasCallStack => Mat -> Mat -> Mat -> Mat
cipher key1 key2 ptext = addRoundKey (shiftRows . subBytes $ ptext'') key15
  where
    f p = addRoundKey (mixColumns . shiftRows . subBytes $ p)
    (key15, schedule) = scheduleHelper (key1, key2)
    ptext' = addRoundKey ptext key1
    ptext'' = foldl' f ptext' schedule

invCipher :: HasCallStack => Mat -> Mat -> Mat -> Mat
invCipher key1 key2 ctext =
  addRoundKey (invSubBytes . invShiftRows $ ctext'') key1
  where
    f k c = invMixColumns $ addRoundKey (invSubBytes . invShiftRows $ c) k
    (key15, schedule) = scheduleHelper (key1, key2)
    ctext' = addRoundKey ctext key15
    ctext'' = foldr f ctext' schedule

-- PUBLIC WRAPPERS

encrypt key ptext =
  toList $ cipher (fromList key1) (fromList key2) (fromList ptext)
  where
    (key1, key2) = splitAt sizeState key

decrypt key ctext =
  toList $ invCipher (fromList key1) (fromList key2) (fromList ctext)
  where
    (key1, key2) = splitAt sizeState key
