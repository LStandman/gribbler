-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMoo/CBCTest.hs: Unit tests for CBC
-- Copyright (C) 2021-2023 LStandman

module Crypt.MooMoo.CBCTest (testAes256Cbc) where

import qualified Crypt.AES256 as AES256
import qualified Crypt.MooMoo.CBC as CBC
import Libtest

{- ORMOLU_DISABLE -}
testAes256Cbc =
  let --  From NIST SP800-38A.
      t1_key =
        [ 0x60, 0x3D, 0xEB, 0x10, 0x15, 0xCA, 0x71, 0xBE,
          0x2B, 0x73, 0xAE, 0xF0, 0x85, 0x7D, 0x77, 0x81,
          0x1F, 0x35, 0x2C, 0x07, 0x3B, 0x61, 0x08, 0xD7,
          0x2D, 0x98, 0x10, 0xA3, 0x09, 0x14, 0xDF, 0xF4
        ]
      t1_iv =
        [ 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
          0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
        ]
      t1_iv_out =
        [ 0xB2, 0xEB, 0x05, 0xE2, 0xC3, 0x9B, 0xE9, 0xFC,
          0xDA, 0x6C, 0x19, 0x07, 0x8C, 0x6A, 0x9D, 0x1B
        ]
      t1_ptext =
        [ 0x6B, 0xC1, 0xBE, 0xE2, 0x2E, 0x40, 0x9F, 0x96,
          0xE9, 0x3D, 0x7E, 0x11, 0x73, 0x93, 0x17, 0x2A,
          0xAE, 0x2D, 0x8A, 0x57, 0x1E, 0x03, 0xAC, 0x9C,
          0x9E, 0xB7, 0x6F, 0xAC, 0x45, 0xAF, 0x8E, 0x51,
          0x30, 0xC8, 0x1C, 0x46, 0xA3, 0x5C, 0xE4, 0x11,
          0xE5, 0xFB, 0xC1, 0x19, 0x1A, 0x0A, 0x52, 0xEF,
          0xF6, 0x9F, 0x24, 0x45, 0xDF, 0x4F, 0x9B, 0x17,
          0xAD, 0x2B, 0x41, 0x7B, 0xE6, 0x6C, 0x37, 0x10
        ]
      t1_ctext =
        [ 0xF5, 0x8C, 0x4C, 0x04, 0xD6, 0xE5, 0xF1, 0xBA,
          0x77, 0x9E, 0xAB, 0xFB, 0x5F, 0x7B, 0xFB, 0xD6,
          0x9C, 0xFC, 0x4E, 0x96, 0x7E, 0xDB, 0x80, 0x8D,
          0x67, 0x9F, 0x77, 0x7B, 0xC6, 0x70, 0x2C, 0x7D,
          0x39, 0xF2, 0x33, 0x69, 0xA9, 0xD9, 0xBA, 0xCF,
          0xA5, 0x30, 0xE2, 0x63, 0x04, 0x23, 0x14, 0x61,
          0xB2, 0xEB, 0x05, 0xE2, 0xC3, 0x9B, 0xE9, 0xFC,
          0xDA, 0x6C, 0x19, 0x07, 0x8C, 0x6A, 0x9D, 0x1B
        ]
      -- From Linux/crypto/testmgr. (Inputs genereated w/ Crypto++.)
      t2_key =
        [ 0xC9, 0x83, 0xA6, 0xC9, 0xEC, 0x0F, 0x32, 0x55,
          0x0F, 0x32, 0x55, 0x78, 0x9B, 0xBE, 0x78, 0x9B,
          0xBE, 0xE1, 0x04, 0x27, 0xE1, 0x04, 0x27, 0x4A,
          0x6D, 0x90, 0x4A, 0x6D, 0x90, 0xB3, 0xD6, 0xF9
        ]
      t2_iv =
        [ 0xE7, 0x82, 0x1D, 0xB8, 0x53, 0x11, 0xAC, 0x47,
          0xE2, 0x7D, 0x18, 0xD6, 0x71, 0x0C, 0xA7, 0x42
        ]
      t2_iv_out =
        [ 0xE0, 0x1F, 0x91, 0xF8, 0x82, 0x96, 0x2D, 0x65,
          0xA3, 0xAA, 0x13, 0xCC, 0x50, 0xFF, 0x7B, 0x02
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
        [ 0xEA, 0x65, 0x8A, 0x19, 0xB0, 0x66, 0xC1, 0x3F,
          0xCE, 0xF1, 0x97, 0x75, 0xC1, 0xFD, 0xB5, 0xAF,
          0x52, 0x65, 0xF7, 0xFF, 0xBC, 0xD8, 0x2D, 0x9F,
          0x2F, 0xB9, 0x26, 0x9B, 0x6F, 0x10, 0xB7, 0xB8,
          0x26, 0xA1, 0x02, 0x46, 0xA2, 0xAD, 0xC6, 0xC0,
          0x11, 0x15, 0xFF, 0x6D, 0x1E, 0x82, 0x04, 0xA6,
          0xB1, 0x74, 0xD1, 0x08, 0x13, 0xFD, 0x90, 0x7C,
          0xF5, 0xED, 0xD3, 0xDB, 0x5A, 0x0A, 0x0C, 0x2F,
          0x0A, 0x70, 0xF1, 0x88, 0x07, 0xCF, 0x21, 0x26,
          0x40, 0x40, 0x8A, 0xF5, 0x53, 0xF7, 0x24, 0x4F,
          0x83, 0x38, 0x43, 0x5F, 0x08, 0x99, 0xEB, 0xE3,
          0xDC, 0x02, 0x64, 0x67, 0x50, 0x6E, 0x15, 0xC3,
          0x01, 0x1A, 0xA0, 0x81, 0x13, 0x65, 0xA6, 0x73,
          0x71, 0xA6, 0x3B, 0x91, 0x83, 0x77, 0xBE, 0xFA,
          0xDB, 0x71, 0x73, 0xA6, 0xC1, 0xAE, 0x43, 0xC3,
          0x36, 0xCE, 0xD6, 0xEB, 0xF9, 0x30, 0x1C, 0x4F,
          0x80, 0x38, 0x5E, 0x9C, 0x6E, 0xAB, 0x98, 0x2F,
          0x53, 0xAF, 0xCF, 0xC8, 0x9A, 0xB8, 0x86, 0x43,
          0x3E, 0x86, 0xE7, 0xA1, 0xF4, 0x2F, 0x30, 0x40,
          0x03, 0xA8, 0x6C, 0x50, 0x42, 0x9F, 0x77, 0x59,
          0x89, 0xA0, 0xC5, 0xEC, 0x9A, 0xB8, 0xDD, 0x99,
          0x16, 0x24, 0x02, 0x07, 0x48, 0xAE, 0xF2, 0x31,
          0x34, 0x0E, 0xC3, 0x85, 0xFE, 0x1C, 0x95, 0x99,
          0x87, 0x58, 0x98, 0x8B, 0xE7, 0xC6, 0xC5, 0x70,
          0x73, 0x81, 0x07, 0x7C, 0x56, 0x2F, 0xD8, 0x1B,
          0xB7, 0xB9, 0x2B, 0xAB, 0xE3, 0x01, 0x87, 0x0F,
          0xD8, 0xBB, 0xC0, 0x0D, 0xAC, 0x2C, 0x2F, 0x98,
          0x3C, 0x0B, 0xA2, 0x99, 0x4A, 0x8C, 0xF7, 0x04,
          0xE0, 0xE0, 0xCF, 0xD1, 0x81, 0x5B, 0xFE, 0xF5,
          0x24, 0x04, 0xFD, 0xB8, 0xDF, 0x13, 0xD8, 0xCD,
          0xF1, 0xE3, 0x3D, 0x98, 0x50, 0x02, 0x77, 0x9E,
          0xBC, 0x22, 0xAB, 0xFA, 0xC2, 0x43, 0x1F, 0x66,
          0x20, 0x02, 0x23, 0xDA, 0xDF, 0xA0, 0x89, 0xF6,
          0xD8, 0xF3, 0x45, 0x24, 0x53, 0x6F, 0x16, 0x77,
          0x02, 0x3E, 0x7B, 0x36, 0x5F, 0xA0, 0x3B, 0x78,
          0x63, 0xA2, 0xBD, 0xB5, 0xA4, 0xCA, 0x1E, 0xD3,
          0x57, 0xBC, 0x0B, 0x9F, 0x43, 0x51, 0x28, 0x4F,
          0x07, 0x50, 0x6C, 0x68, 0x12, 0x07, 0xCF, 0xFA,
          0x6B, 0x72, 0x0B, 0xEB, 0xF8, 0x88, 0x90, 0x2C,
          0x7E, 0xF5, 0x91, 0xD1, 0x03, 0xD8, 0xD5, 0xBD,
          0x22, 0x39, 0x7B, 0x16, 0x03, 0x01, 0x69, 0xAF,
          0x3D, 0x38, 0x66, 0x28, 0x0C, 0xBE, 0x5B, 0xC5,
          0x03, 0xB4, 0x2F, 0x51, 0x8A, 0x56, 0x17, 0x2B,
          0x88, 0x42, 0x6D, 0x40, 0x68, 0x8F, 0xD0, 0x11,
          0x19, 0xF9, 0x1F, 0x43, 0x79, 0x95, 0x31, 0xFA,
          0x28, 0x7A, 0x3D, 0xF7, 0x66, 0xEB, 0xEF, 0xAC,
          0x06, 0xB2, 0x01, 0xAD, 0xDB, 0x68, 0xDB, 0xEC,
          0x8D, 0x53, 0x6E, 0x72, 0x68, 0xA3, 0xC7, 0x63,
          0x43, 0x2B, 0x78, 0xE0, 0x04, 0x29, 0x8F, 0x72,
          0xB2, 0x2C, 0xE6, 0x84, 0x03, 0x30, 0x6D, 0xCD,
          0x26, 0x92, 0x37, 0xE1, 0x2F, 0xBB, 0x8B, 0x9D,
          0xE4, 0x4C, 0xF6, 0x93, 0xBC, 0xD9, 0xAD, 0x44,
          0x52, 0x65, 0xC7, 0xB0, 0x0E, 0x3F, 0x0E, 0x61,
          0x56, 0x5D, 0x1C, 0x6D, 0xA7, 0x05, 0x2E, 0xBC,
          0x58, 0x08, 0x15, 0xAB, 0x12, 0xAB, 0x17, 0x4A,
          0x5E, 0x1C, 0xF2, 0xCD, 0xB8, 0xA2, 0xAE, 0xFB,
          0x9B, 0x2E, 0x0E, 0x85, 0x34, 0x80, 0x0E, 0x3F,
          0x4C, 0xB8, 0xDB, 0xCE, 0x1C, 0x90, 0xA1, 0x61,
          0x6C, 0x69, 0x09, 0x35, 0x9E, 0xD4, 0xF4, 0xAD,
          0xBC, 0x06, 0x41, 0xE3, 0x01, 0xB4, 0x4E, 0x0A,
          0xE0, 0x1F, 0x91, 0xF8, 0x82, 0x96, 0x2D, 0x65,
          0xA3, 0xAA, 0x13, 0xCC, 0x50, 0xFF, 0x7B, 0x02
        ]
   in testsuite
        "AES256CBC"
        [ test
            "EncryptNISTSP80038A"
            [ expectVarEq "t1_ctext" (t1_ctext, t1_iv_out) $
                CBC.encrypt1
                  AES256.sizeBlock
                  (AES256.encrypt t1_key)
                  t1_iv
                  t1_ptext
            ],
          test
            "EncryptTestmgr"
            [ expectVarEq "t2_ctext" (t2_ctext, t2_iv_out) $
                CBC.encrypt1
                  AES256.sizeBlock
                  (AES256.encrypt t2_key)
                  t2_iv
                  t2_ptext
            ],
          test
            "DecryptNISTSP80038A"
            [ expectMemEq "t1_ptext" t1_ptext $
                CBC.decrypt
                  AES256.sizeBlock
                  (AES256.decrypt t1_key)
                  t1_iv
                  t1_ctext
            ],
          test
            "DecryptTestmgr"
            [ expectMemEq "t2_ptext" t2_ptext $
                CBC.decrypt
                  AES256.sizeBlock
                  (AES256.decrypt t2_key)
                  t2_iv
                  t2_ctext
            ]
        ]
