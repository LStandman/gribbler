-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/AES256Test.hs: Unit tests for AES256
-- Copyright (C) 2021-2023 LStandman

module Crypt.AES256Test (testAes256) where

import qualified Crypt.AES256 as AES256
import Data.Word
import Libtest

nomodAes256 :: Bool -> [Word8] -> [Word8] -> [Word8]
nomodAes256 _ [] _ = []
nomodAes256 isEnc text key =
  out ++ nomodAes256 isEnc text'' key
  where
    (text', text'') = splitAt AES256.sizeBlock text
    out
      | isEnc = AES256.encrypt text' key
      | otherwise = AES256.decrypt text' key

{- ORMOLU_DISABLE -}
testAes256 =
  let --  From FIPS-197
      t1_key   =
        [ 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
          0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
          0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
          0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F
        ]
      t1_ptext =
        [ 0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
          0x88, 0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF
        ]
      t1_ctext =
        [ 0x8E, 0xA2, 0xB7, 0xCA, 0x51, 0x67, 0x45, 0xBF,
          0xEA, 0xFC, 0x49, 0x90, 0x4B, 0x49, 0x60, 0x89
        ]
      -- From Linux/crypto/testmgr. (Inputs genereated w/ Crypto++.)
      t2_key   =
        [ 0xA6, 0xC9, 0x83, 0xA6, 0xC9, 0xEC, 0x0F, 0x32,
          0x55, 0x0F, 0x32, 0x55, 0x78, 0x9B, 0xBE, 0x78,
          0x9B, 0xBE, 0xE1, 0x04, 0x27, 0xE1, 0x04, 0x27,
          0x4A, 0x6D, 0x90, 0x4A, 0x6D, 0x90, 0xB3, 0xD6
        ]
      t2_ptext =
        [ 0x50, 0xB9, 0x22, 0xAE, 0x17, 0x80, 0x0C, 0x75,
          0xDE, 0x47, 0xD3, 0x3C, 0xA5, 0x0E, 0x9A, 0x03,
          0x6C, 0xF8, 0x61, 0xCA, 0x33, 0xBF, 0x28, 0x91,
          0x1D, 0x86, 0xEF, 0x58, 0xE4, 0x4D, 0xB6, 0x1F,
          0xAB, 0x14, 0x7D, 0x09, 0x72, 0xDB, 0x44, 0xD0,
          0x39, 0xA2, 0x0B, 0x97, 0x00, 0x69, 0xF5, 0x5E,
          0xC7, 0x30, 0xBC, 0x25, 0x8E, 0x1A, 0x83, 0xEC,
          0x55, 0xE1, 0x4A, 0xB3, 0x1C, 0xA8, 0x11, 0x7A,
          0x06, 0x6F, 0xD8, 0x41, 0xCD, 0x36, 0x9F, 0x08,
          0x94, 0xFD, 0x66, 0xF2, 0x5B, 0xC4, 0x2D, 0xB9,
          0x22, 0x8B, 0x17, 0x80, 0xE9, 0x52, 0xDE, 0x47,
          0xB0, 0x19, 0xA5, 0x0E, 0x77, 0x03, 0x6C, 0xD5,
          0x3E, 0xCA, 0x33, 0x9C, 0x05, 0x91, 0xFA, 0x63,
          0xEF, 0x58, 0xC1, 0x2A, 0xB6, 0x1F, 0x88, 0x14,
          0x7D, 0xE6, 0x4F, 0xDB, 0x44, 0xAD, 0x16, 0xA2,
          0x0B, 0x74, 0x00, 0x69, 0xD2, 0x3B, 0xC7, 0x30,
          0x99, 0x02, 0x8E, 0xF7, 0x60, 0xEC, 0x55, 0xBE,
          0x27, 0xB3, 0x1C, 0x85, 0x11, 0x7A, 0xE3, 0x4C,
          0xD8, 0x41, 0xAA, 0x13, 0x9F, 0x08, 0x71, 0xFD,
          0x66, 0xCF, 0x38, 0xC4, 0x2D, 0x96, 0x22, 0x8B,
          0xF4, 0x5D, 0xE9, 0x52, 0xBB, 0x24, 0xB0, 0x19,
          0x82, 0x0E, 0x77, 0xE0, 0x49, 0xD5, 0x3E, 0xA7,
          0x10, 0x9C, 0x05, 0x6E, 0xFA, 0x63, 0xCC, 0x35,
          0xC1, 0x2A, 0x93, 0x1F, 0x88, 0xF1, 0x5A, 0xE6,
          0x4F, 0xB8, 0x21, 0xAD, 0x16, 0x7F, 0x0B, 0x74,
          0xDD, 0x46, 0xD2, 0x3B, 0xA4, 0x0D, 0x99, 0x02,
          0x6B, 0xF7, 0x60, 0xC9, 0x32, 0xBE, 0x27, 0x90,
          0x1C, 0x85, 0xEE, 0x57, 0xE3, 0x4C, 0xB5, 0x1E,
          0xAA, 0x13, 0x7C, 0x08, 0x71, 0xDA, 0x43, 0xCF,
          0x38, 0xA1, 0x0A, 0x96, 0xFF, 0x68, 0xF4, 0x5D,
          0xC6, 0x2F, 0xBB, 0x24, 0x8D, 0x19, 0x82, 0xEB,
          0x54, 0xE0, 0x49, 0xB2, 0x1B, 0xA7, 0x10, 0x79,
          0x05, 0x6E, 0xD7, 0x40, 0xCC, 0x35, 0x9E, 0x07,
          0x93, 0xFC, 0x65, 0xF1, 0x5A, 0xC3, 0x2C, 0xB8,
          0x21, 0x8A, 0x16, 0x7F, 0xE8, 0x51, 0xDD, 0x46,
          0xAF, 0x18, 0xA4, 0x0D, 0x76, 0x02, 0x6B, 0xD4,
          0x3D, 0xC9, 0x32, 0x9B, 0x04, 0x90, 0xF9, 0x62,
          0xEE, 0x57, 0xC0, 0x29, 0xB5, 0x1E, 0x87, 0x13,
          0x7C, 0xE5, 0x4E, 0xDA, 0x43, 0xAC, 0x15, 0xA1,
          0x0A, 0x73, 0xFF, 0x68, 0xD1, 0x3A, 0xC6, 0x2F,
          0x98, 0x01, 0x8D, 0xF6, 0x5F, 0xEB, 0x54, 0xBD,
          0x26, 0xB2, 0x1B, 0x84, 0x10, 0x79, 0xE2, 0x4B,
          0xD7, 0x40, 0xA9, 0x12, 0x9E, 0x07, 0x70, 0xFC,
          0x65, 0xCE, 0x37, 0xC3, 0x2C, 0x95, 0x21, 0x8A,
          0xF3, 0x5C, 0xE8, 0x51, 0xBA, 0x23, 0xAF, 0x18,
          0x81, 0x0D, 0x76, 0xDF, 0x48, 0xD4, 0x3D, 0xA6,
          0x0F, 0x9B, 0x04, 0x6D, 0xF9, 0x62, 0xCB, 0x34,
          0xC0, 0x29, 0x92, 0x1E, 0x87, 0xF0, 0x59, 0xE5,
          0x4E, 0xB7, 0x20, 0xAC, 0x15, 0x7E, 0x0A, 0x73,
          0xDC, 0x45, 0xD1, 0x3A, 0xA3, 0x0C, 0x98, 0x01,
          0x6A, 0xF6, 0x5F, 0xC8, 0x31, 0xBD, 0x26, 0x8F,
          0x1B, 0x84, 0xED, 0x56, 0xE2, 0x4B, 0xB4, 0x1D,
          0xA9, 0x12, 0x7B, 0x07, 0x70, 0xD9, 0x42, 0xCE,
          0x37, 0xA0, 0x09, 0x95, 0xFE, 0x67, 0xF3, 0x5C,
          0xC5, 0x2E, 0xBA, 0x23, 0x8C, 0x18, 0x81, 0xEA,
          0x53, 0xDF, 0x48, 0xB1, 0x1A, 0xA6, 0x0F, 0x78,
          0x04, 0x6D, 0xD6, 0x3F, 0xCB, 0x34, 0x9D, 0x06,
          0x92, 0xFB, 0x64, 0xF0, 0x59, 0xC2, 0x2B, 0xB7,
          0x20, 0x89, 0x15, 0x7E, 0xE7, 0x50, 0xDC, 0x45,
          0xAE, 0x17, 0xA3, 0x0C, 0x75, 0x01, 0x6A, 0xD3,
          0x3C, 0xC8, 0x31, 0x9A, 0x03, 0x8F, 0xF8, 0x61,
          0xED, 0x56, 0xBF, 0x28, 0xB4, 0x1D, 0x86, 0x12
        ]
      t2_ctext =
        [ 0x71, 0x73, 0xF7, 0xDB, 0x24, 0x93, 0x21, 0x6D,
          0x61, 0x1E, 0xBB, 0x63, 0x42, 0x79, 0xDB, 0x64,
          0x6F, 0x82, 0xC0, 0xCA, 0xA3, 0x9B, 0xFA, 0x0B,
          0xD9, 0x08, 0xC7, 0x4A, 0x90, 0xAE, 0x8F, 0x5F,
          0x5E, 0x06, 0xF0, 0x5F, 0x31, 0x51, 0x18, 0x37,
          0x45, 0xD7, 0xCA, 0x3A, 0xFD, 0x6C, 0x3F, 0xE1,
          0xDD, 0x8D, 0x22, 0x65, 0x2B, 0x00, 0x50, 0xCE,
          0xBA, 0x28, 0x67, 0xD7, 0xCE, 0x0E, 0x0D, 0xEA,
          0x78, 0x69, 0x7F, 0xAE, 0x8F, 0x8B, 0x69, 0x37,
          0x75, 0xE0, 0xDC, 0x96, 0xE0, 0xB7, 0xF4, 0x09,
          0xCB, 0x6D, 0xA2, 0xFB, 0xDA, 0xAF, 0x09, 0xF8,
          0x81, 0x82, 0x27, 0xFA, 0x45, 0x9C, 0x29, 0xA4,
          0x22, 0x8B, 0x78, 0x69, 0x5B, 0x46, 0xF9, 0x39,
          0x1B, 0xCC, 0xF9, 0x1D, 0x09, 0xEB, 0xBC, 0x5C,
          0x41, 0x72, 0x51, 0x97, 0x1D, 0x07, 0x49, 0xA0,
          0x1B, 0x8E, 0x65, 0x4B, 0xB2, 0x6A, 0x12, 0x03,
          0x6A, 0x60, 0x95, 0xAC, 0xBD, 0xAC, 0x1A, 0x64,
          0xDE, 0x5A, 0xA5, 0xF0, 0x83, 0x2F, 0xCB, 0xCA,
          0x22, 0x74, 0xA6, 0x6C, 0x9B, 0x73, 0xCE, 0x3F,
          0xE1, 0x8B, 0x22, 0x17, 0x59, 0x0C, 0x47, 0x89,
          0x33, 0xA1, 0xD6, 0x47, 0x03, 0x19, 0x4F, 0xA8,
          0x67, 0x69, 0xF0, 0x5B, 0xF0, 0x20, 0xAD, 0x06,
          0x27, 0x81, 0x92, 0xD8, 0xC5, 0xBA, 0x98, 0x12,
          0xBE, 0x24, 0xB5, 0x2F, 0x75, 0x02, 0xC2, 0xAD,
          0x12, 0x2F, 0x07, 0x32, 0xEE, 0x39, 0xAF, 0x64,
          0x05, 0x8F, 0xB3, 0xD4, 0xEB, 0x1B, 0x46, 0x6E,
          0xD9, 0x21, 0xF9, 0xC4, 0xB7, 0xC9, 0x45, 0x68,
          0xB4, 0xA1, 0x74, 0x9F, 0x82, 0x47, 0xEB, 0xCC,
          0xBD, 0x0A, 0x14, 0x95, 0x0F, 0x8B, 0xA8, 0x2F,
          0x4B, 0x1B, 0xA7, 0xBF, 0x82, 0xA6, 0x43, 0x0C,
          0xB9, 0x39, 0x4A, 0xA8, 0x10, 0x6F, 0x50, 0x7B,
          0x25, 0xFB, 0x26, 0x81, 0xE0, 0x2F, 0xF0, 0x96,
          0x8D, 0x8B, 0xAC, 0x92, 0x0F, 0xF6, 0xED, 0x64,
          0x63, 0x29, 0x4C, 0x8E, 0x18, 0x13, 0xC5, 0xBF,
          0xFC, 0xA0, 0xD9, 0xBF, 0x7C, 0x3A, 0x0E, 0x29,
          0x6F, 0xD1, 0x6C, 0x6F, 0xA5, 0xDA, 0xBF, 0xB1,
          0x30, 0xEA, 0x44, 0x2D, 0xC3, 0x8F, 0x16, 0xE1,
          0x66, 0xFA, 0xA3, 0x21, 0x3E, 0xFC, 0x13, 0xCA,
          0xF0, 0xF6, 0xF0, 0x59, 0xBD, 0x8F, 0x38, 0x50,
          0x31, 0xCB, 0x69, 0x3F, 0x96, 0x15, 0xD6, 0xF5,
          0xAE, 0xFF, 0xF6, 0xAA, 0x41, 0x85, 0x4C, 0x10,
          0x58, 0xE3, 0xF9, 0x44, 0xE6, 0x28, 0xDA, 0x9A,
          0xDC, 0x6A, 0x80, 0x34, 0x73, 0x97, 0x1B, 0xC5,
          0xCA, 0x26, 0x16, 0x77, 0x0E, 0x60, 0xAB, 0x89,
          0x0F, 0x04, 0x27, 0xBD, 0xCE, 0x3E, 0x71, 0xB4,
          0xA0, 0xD7, 0x22, 0x7E, 0xDB, 0xEB, 0x24, 0x70,
          0x42, 0x71, 0x51, 0x78, 0x70, 0xB3, 0xE0, 0x3D,
          0x84, 0x8E, 0x8D, 0x7B, 0xD0, 0x6D, 0xEA, 0x92,
          0x11, 0x08, 0x42, 0x4F, 0xE5, 0xAD, 0x26, 0x92,
          0xD2, 0x00, 0xAE, 0xA8, 0xE3, 0x4B, 0x37, 0x47,
          0x22, 0xC1, 0x95, 0xC1, 0x63, 0x7F, 0xCB, 0x03,
          0xF3, 0xE3, 0xD7, 0x9D, 0x60, 0xC7, 0xBC, 0xEA,
          0x35, 0xA2, 0xFD, 0x45, 0x52, 0x39, 0x13, 0x6F,
          0xC1, 0x53, 0xF3, 0x53, 0xDF, 0x33, 0x84, 0xD7,
          0xD2, 0xC8, 0x37, 0xB0, 0x75, 0xE3, 0x41, 0x46,
          0xB3, 0xC7, 0x83, 0x2E, 0x8A, 0xBB, 0xA4, 0xE5,
          0x7F, 0x3C, 0xFD, 0x8B, 0xEB, 0xEA, 0x63, 0xBD,
          0xB7, 0x46, 0xE7, 0xBF, 0x09, 0x9C, 0x0D, 0x0F,
          0x40, 0x86, 0x7F, 0x51, 0xE1, 0x11, 0x9C, 0xCB,
          0x88, 0xE6, 0x68, 0x47, 0xE3, 0x2B, 0xC5, 0xFF,
          0x09, 0x79, 0xA0, 0x43, 0x5C, 0x0D, 0x08, 0x58,
          0x17, 0xBB, 0xC0, 0x6B, 0x62, 0x3F, 0x56, 0xE9
        ]
   in testsuite
        "AES256NoMod"
        [ test
            "EncryptFIPS197"
            [ expectMemEq "t1_ctext" t1_ctext $
                nomodAes256 True t1_ptext t1_key
            ],
          test
            "EncryptTestmgr"
            [ expectMemEq "t2_ctext" t2_ctext $
                nomodAes256 True t2_ptext t2_key
            ],
          test
            "DecryptFIPS197"
            [ expectMemEq "t1_ptext" t1_ptext $
                nomodAes256 False t1_ctext t1_key
            ],
          test
            "DecryptTestmgr"
            [ expectMemEq "t2_ptext" t2_ptext $
                nomodAes256 False t2_ctext t2_key
            ]
        ]
