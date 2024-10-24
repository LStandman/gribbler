-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMoo/GCMTest.hs: Unit tests for GCM
-- Copyright (C) 2024 LStandman

module Crypt.MooMoo.GCMTest (testAes256Gcm) where

import qualified Crypt.AES256 as AES256
import qualified Crypt.MooMoo.GCM as GCM
import Data.Either
import Libtest

{- ORMOLU_DISABLE -}
testAes256Gcm =
  let -- from bn-randall-test-vectors-0511-v1.pdf
      randallTests =
        [ ( [ 0xE3, 0xC0, 0x8A, 0x8F, 0x06, 0xC6, 0xE3, 0xAD,
              0x95, 0xA7, 0x05, 0x57, 0xB2, 0x3F, 0x75, 0x48,
              0x3C, 0xE3, 0x30, 0x21, 0xA9, 0xC7, 0x2B, 0x70,
              0x25, 0x66, 0x62, 0x04, 0xC6, 0x9C, 0x0B, 0x72
            ],
            [ 0x12, 0x15, 0x35, 0x24, 0xC0, 0x89, 0x5E, 0x81,
              0xB2, 0xC2, 0x84, 0x65
            ],
            [ 0xD6, 0x09, 0xB1, 0xF0, 0x56, 0x63, 0x7A, 0x0D,
              0x46, 0xDF, 0x99, 0x8D, 0x88, 0xE5, 0x22, 0x2A,
              0xB2, 0xC2, 0x84, 0x65, 0x12, 0x15, 0x35, 0x24,
              0xC0, 0x89, 0x5E, 0x81, 0x08, 0x00, 0x0F, 0x10,
              0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
              0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
              0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
              0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30,
              0x31, 0x32, 0x33, 0x34, 0x00, 0x01
            ],
            [ ],
            ( [ ],
              [ 0x2F, 0x0B, 0xC5, 0xAF, 0x40, 0x9E, 0x06, 0xD6,
                0x09, 0xEA, 0x8B, 0x7D, 0x0F, 0xA5, 0xEA, 0x50
              ]
            )
          ),
          ( [ 0xE3, 0xC0, 0x8A, 0x8F, 0x06, 0xC6, 0xE3, 0xAD,
              0x95, 0xA7, 0x05, 0x57, 0xB2, 0x3F, 0x75, 0x48,
              0x3C, 0xE3, 0x30, 0x21, 0xA9, 0xC7, 0x2B, 0x70,
              0x25, 0x66, 0x62, 0x04, 0xC6, 0x9C, 0x0B, 0x72
            ],
            [ 0x12, 0x15, 0x35, 0x24, 0xC0, 0x89, 0x5E, 0x81,
              0xB2, 0xC2, 0x84, 0x65
            ],
            [ 0xD6, 0x09, 0xB1, 0xF0, 0x56, 0x63, 0x7A, 0x0D,
              0x46, 0xDF, 0x99, 0x8D, 0x88, 0xE5, 0x2E, 0x00,
              0xB2, 0xC2, 0x84, 0x65, 0x12, 0x15, 0x35, 0x24,
              0xC0, 0x89, 0x5E, 0x81
            ],
            [ 0x08, 0x00, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14,
              0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C,
              0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24,
              0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C,
              0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34,
              0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x00, 0x02
            ],
            ( [ 0xE2, 0x00, 0x6E, 0xB4, 0x2F, 0x52, 0x77, 0x02,
                0x2D, 0x9B, 0x19, 0x92, 0x5B, 0xC4, 0x19, 0xD7,
                0xA5, 0x92, 0x66, 0x6C, 0x92, 0x5F, 0xE2, 0xEF,
                0x71, 0x8E, 0xB4, 0xE3, 0x08, 0xEF, 0xEA, 0xA7,
                0xC5, 0x27, 0x3B, 0x39, 0x41, 0x18, 0x86, 0x0A,
                0x5B, 0xE2, 0xA9, 0x7F, 0x56, 0xAB, 0x78, 0x36
              ],
              [ 0x5C, 0xA5, 0x97, 0xCD, 0xBB, 0x3E, 0xDB, 0x8D,
                0x1A, 0x11, 0x51, 0xEA, 0x0A, 0xF7, 0xB4, 0x36
              ]
            )
          ),
          ( [ 0x69, 0x1D, 0x3E, 0xE9, 0x09, 0xD7, 0xF5, 0x41,
              0x67, 0xFD, 0x1C, 0xA0, 0xB5, 0xD7, 0x69, 0x08,
              0x1F, 0x2B, 0xDE, 0x1A, 0xEE, 0x65, 0x5F, 0xDB,
              0xAB, 0x80, 0xBD, 0x52, 0x95, 0xAE, 0x6B, 0xE7
            ],
            [ 0xF0, 0x76, 0x1E, 0x8D, 0xCD, 0x3D, 0x00, 0x01,
              0x76, 0xD4, 0x57, 0xED
            ],
            [ 0xE2, 0x01, 0x06, 0xD7, 0xCD, 0x0D, 0xF0, 0x76,
              0x1E, 0x8D, 0xCD, 0x3D, 0x88, 0xE5, 0x40, 0x00,
              0x76, 0xD4, 0x57, 0xED, 0x08, 0x00, 0x0F, 0x10,
              0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
              0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
              0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
              0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30,
              0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
              0x39, 0x3A, 0x00, 0x03
            ],
            [ ],
            ( [ ],
              [ 0x35, 0x21, 0x7C, 0x77, 0x4B, 0xBC, 0x31, 0xB6,
                0x31, 0x66, 0xBC, 0xF9, 0xD4, 0xAB, 0xED, 0x07
              ]
            )
          ),
          ( [ 0x69, 0x1D, 0x3E, 0xE9, 0x09, 0xD7, 0xF5, 0x41,
              0x67, 0xFD, 0x1C, 0xA0, 0xB5, 0xD7, 0x69, 0x08,
              0x1F, 0x2B, 0xDE, 0x1A, 0xEE, 0x65, 0x5F, 0xDB,
              0xAB, 0x80, 0xBD, 0x52, 0x95, 0xAE, 0x6B, 0xE7
            ],
            [ 0xF0, 0x76, 0x1E, 0x8D, 0xCD, 0x3D, 0x00, 0x01,
              0x76, 0xD4, 0x57, 0xED
            ],
            [ 0xE2, 0x01, 0x06, 0xD7, 0xCD, 0x0D, 0xF0, 0x76,
              0x1E, 0x8D, 0xCD, 0x3D, 0x88, 0xE5, 0x4C, 0x2A,
              0x76, 0xD4, 0x57, 0xED
            ],
            [ 0x08, 0x00, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14,
              0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C,
              0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24,
              0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C,
              0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34,
              0x00, 0x04
            ],
            ( [ 0xC1, 0x62, 0x3F, 0x55, 0x73, 0x0C, 0x93, 0x53,
                0x30, 0x97, 0xAD, 0xDA, 0xD2, 0x56, 0x64, 0x96,
                0x61, 0x25, 0x35, 0x2B, 0x43, 0xAD, 0xAC, 0xBD,
                0x61, 0xC5, 0xEF, 0x3A, 0xC9, 0x0B, 0x5B, 0xEE,
                0x92, 0x9C, 0xE4, 0x63, 0x0E, 0xA7, 0x9F, 0x6C,
                0xE5, 0x19
              ],
              [ 0x12, 0xAF, 0x39, 0xC2, 0xD1, 0xFD, 0xC2, 0x05,
                0x1F, 0x8B, 0x7B, 0x3C, 0x9D, 0x39, 0x7E, 0xF2
              ]
            )
          ),
          ( [ 0x83, 0xC0, 0x93, 0xB5, 0x8D, 0xE7, 0xFF, 0xE1,
              0xC0, 0xDA, 0x92, 0x6A, 0xC4, 0x3F, 0xB3, 0x60,
              0x9A, 0xC1, 0xC8, 0x0F, 0xEE, 0x1B, 0x62, 0x44,
              0x97, 0xEF, 0x94, 0x2E, 0x2F, 0x79, 0xA8, 0x23
            ],
            [ 0x7C, 0xFD, 0xE9, 0xF9, 0xE3, 0x37, 0x24, 0xC6,
              0x89, 0x32, 0xD6, 0x12
            ],
            [ 0x84, 0xC5, 0xD5, 0x13, 0xD2, 0xAA, 0xF6, 0xE5,
              0xBB, 0xD2, 0x72, 0x77, 0x88, 0xE5, 0x23, 0x00,
              0x89, 0x32, 0xD6, 0x12, 0x7C, 0xFD, 0xE9, 0xF9,
              0xE3, 0x37, 0x24, 0xC6, 0x08, 0x00, 0x0F, 0x10,
              0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
              0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
              0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
              0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30,
              0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
              0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x00,
              0x05
            ],
            [ ],
            ( [ ],
              [ 0x6E, 0xE1, 0x60, 0xE8, 0xFA, 0xEC, 0xA4, 0xB3,
                0x6C, 0x86, 0xB2, 0x34, 0x92, 0x0C, 0xA9, 0x75
              ]
            )
          ),
          ( [ 0x83, 0xC0, 0x93, 0xB5, 0x8D, 0xE7, 0xFF, 0xE1,
              0xC0, 0xDA, 0x92, 0x6A, 0xC4, 0x3F, 0xB3, 0x60,
              0x9A, 0xC1, 0xC8, 0x0F, 0xEE, 0x1B, 0x62, 0x44,
              0x97, 0xEF, 0x94, 0x2E, 0x2F, 0x79, 0xA8, 0x23
            ],
            [ 0x7C, 0xFD, 0xE9, 0xF9, 0xE3, 0x37, 0x24, 0xC6,
              0x89, 0x32, 0xD6, 0x12
            ],
            [ 0x84, 0xC5, 0xD5, 0x13, 0xD2, 0xAA, 0xF6, 0xE5,
              0xBB, 0xD2, 0x72, 0x77, 0x88, 0xE5, 0x2F, 0x00,
              0x89, 0x32, 0xD6, 0x12, 0x7C, 0xFD, 0xE9, 0xF9,
              0xE3, 0x37, 0x24, 0xC6
            ],
            [ 0x08, 0x00, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14,
              0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C,
              0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24,
              0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C,
              0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34,
              0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x00,
              0x06
            ],
            ( [ 0x11, 0x02, 0x22, 0xFF, 0x80, 0x50, 0xCB, 0xEC,
                0xE6, 0x6A, 0x81, 0x3A, 0xD0, 0x9A, 0x73, 0xED,
                0x7A, 0x9A, 0x08, 0x9C, 0x10, 0x6B, 0x95, 0x93,
                0x89, 0x16, 0x8E, 0xD6, 0xE8, 0x69, 0x8E, 0xA9,
                0x02, 0xEB, 0x12, 0x77, 0xDB, 0xEC, 0x2E, 0x68,
                0xE4, 0x73, 0x15, 0x5A, 0x15, 0xA7, 0xDA, 0xEE,
                0xD4
              ],
              [ 0xA1, 0x0F, 0x4E, 0x05, 0x13, 0x9C, 0x23, 0xDF,
                0x00, 0xB3, 0xAA, 0xDC, 0x71, 0xF0, 0x59, 0x6A
              ]
            )
          ),
          ( [ 0x4C, 0x97, 0x3D, 0xBC, 0x73, 0x64, 0x62, 0x16,
              0x74, 0xF8, 0xB5, 0xB8, 0x9E, 0x5C, 0x15, 0x51,
              0x1F, 0xCE, 0xD9, 0x21, 0x64, 0x90, 0xFB, 0x1C,
              0x1A, 0x2C, 0xAA, 0x0F, 0xFE, 0x04, 0x07, 0xE5
            ],
            [ 0x7A, 0xE8, 0xE2, 0xCA, 0x4E, 0xC5, 0x00, 0x01,
              0x2E, 0x58, 0x49, 0x5C
            ],
            [ 0x68, 0xF2, 0xE7, 0x76, 0x96, 0xCE, 0x7A, 0xE8,
              0xE2, 0xCA, 0x4E, 0xC5, 0x88, 0xE5, 0x41, 0x00,
              0x2E, 0x58, 0x49, 0x5C, 0x08, 0x00, 0x0F, 0x10,
              0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
              0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
              0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
              0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30,
              0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,
              0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40,
              0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
              0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x00, 0x07
            ],
            [ ],
            ( [ ],
              [ 0x00, 0xBD, 0xA1, 0xB7, 0xE8, 0x76, 0x08, 0xBC,
                0xBF, 0x47, 0x0F, 0x12, 0x15, 0x7F, 0x4C, 0x07
              ]
            )
          ),
          ( [ 0x4C, 0x97, 0x3D, 0xBC, 0x73, 0x64, 0x62, 0x16,
              0x74, 0xF8, 0xB5, 0xB8, 0x9E, 0x5C, 0x15, 0x51,
              0x1F, 0xCE, 0xD9, 0x21, 0x64, 0x90, 0xFB, 0x1C,
              0x1A, 0x2C, 0xAA, 0x0F, 0xFE, 0x04, 0x07, 0xE5
            ],
            [ 0x7A, 0xE8, 0xE2, 0xCA, 0x4E, 0xC5, 0x00, 0x01,
              0x2E, 0x58, 0x49, 0x5C
            ],
            [ 0x68, 0xF2, 0xE7, 0x76, 0x96, 0xCE, 0x7A, 0xE8,
              0xE2, 0xCA, 0x4E, 0xC5, 0x88, 0xE5, 0x4D, 0x00,
              0x2E, 0x58, 0x49, 0x5C
            ],
            [ 0x08, 0x00, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14,
              0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C,
              0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24,
              0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C,
              0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34,
              0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C,
              0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44,
              0x45, 0x46, 0x47, 0x48, 0x49, 0x00, 0x08
            ],
            ( [ 0xBA, 0x8A, 0xE3, 0x1B, 0xC5, 0x06, 0x48, 0x6D,
                0x68, 0x73, 0xE4, 0xFC, 0xE4, 0x60, 0xE7, 0xDC,
                0x57, 0x59, 0x1F, 0xF0, 0x06, 0x11, 0xF3, 0x1C,
                0x38, 0x34, 0xFE, 0x1C, 0x04, 0xAD, 0x80, 0xB6,
                0x68, 0x03, 0xAF, 0xCF, 0x5B, 0x27, 0xE6, 0x33,
                0x3F, 0xA6, 0x7C, 0x99, 0xDA, 0x47, 0xC2, 0xF0,
                0xCE, 0xD6, 0x8D, 0x53, 0x1B, 0xD7, 0x41, 0xA9,
                0x43, 0xCF, 0xF7, 0xA6, 0x71, 0x3B, 0xD0
              ],
              [ 0x26, 0x11, 0xCD, 0x7D, 0xAA, 0x01, 0xD6, 0x1C,
                0x5C, 0x88, 0x6D, 0xC1, 0xA8, 0x17, 0x01, 0x07
              ]
            )
          )
        ]
      -- From McGrew & Viega 2005.
      mv2005Tests =
        [ ( [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            ],
            [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00
            ],
            [ ],
            [ ],
            ( [ ],
              [ 0x53, 0x0F, 0x8A, 0xFB, 0xC7, 0x45, 0x36, 0xB9,
                0xA9, 0x63, 0xB4, 0xF1, 0xC4, 0xCB, 0x73, 0x8B
              ]
            )
          ),
          ( [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            ],
            [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00
            ],
            [ ],
            [ 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            ],
            ( [ 0xCE, 0xA7, 0x40, 0x3D, 0x4D, 0x60, 0x6B, 0x6E,
                0x07, 0x4E, 0xC5, 0xD3, 0xBA, 0xF3, 0x9D, 0x18
              ],
              [ 0xD0, 0xD1, 0xC8, 0xA7, 0x99, 0x99, 0x6B, 0xF0,
                0x26, 0x5B, 0x98, 0xB5, 0xD4, 0x8A, 0xB9, 0x19
              ]
            )
          ),
          ( [ 0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08,
              0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08
            ],
            [ 0xCA, 0xFE, 0xBA, 0xBE, 0xFA, 0xCE, 0xDB, 0xAD,
              0xDE, 0xCA, 0xF8, 0x88
            ],
            [ ],
            [ 0xD9, 0x31, 0x32, 0x25, 0xF8, 0x84, 0x06, 0xE5,
              0xA5, 0x59, 0x09, 0xC5, 0xAF, 0xF5, 0x26, 0x9A,
              0x86, 0xA7, 0xA9, 0x53, 0x15, 0x34, 0xF7, 0xDA,
              0x2E, 0x4C, 0x30, 0x3D, 0x8A, 0x31, 0x8A, 0x72,
              0x1C, 0x3C, 0x0C, 0x95, 0x95, 0x68, 0x09, 0x53,
              0x2F, 0xCF, 0x0E, 0x24, 0x49, 0xA6, 0xB5, 0x25,
              0xB1, 0x6A, 0xED, 0xF5, 0xAA, 0x0D, 0xE6, 0x57,
              0xBA, 0x63, 0x7B, 0x39, 0x1A, 0xAF, 0xD2, 0x55
            ],
            ( [ 0x52, 0x2D, 0xC1, 0xF0, 0x99, 0x56, 0x7D, 0x07,
                0xF4, 0x7F, 0x37, 0xA3, 0x2A, 0x84, 0x42, 0x7D,
                0x64, 0x3A, 0x8C, 0xDC, 0xBF, 0xE5, 0xC0, 0xC9,
                0x75, 0x98, 0xA2, 0xBD, 0x25, 0x55, 0xD1, 0xAA,
                0x8C, 0xB0, 0x8E, 0x48, 0x59, 0x0D, 0xBB, 0x3D,
                0xA7, 0xB0, 0x8B, 0x10, 0x56, 0x82, 0x88, 0x38,
                0xC5, 0xF6, 0x1E, 0x63, 0x93, 0xBA, 0x7A, 0x0A,
                0xBC, 0xC9, 0xF6, 0x62, 0x89, 0x80, 0x15, 0xAD
              ],
              [ 0xB0, 0x94, 0xDA, 0xC5, 0xD9, 0x34, 0x71, 0xBD,
                0xEC, 0x1A, 0x50, 0x22, 0x70, 0xE3, 0xCC, 0x6C
              ]
            )
          ),
          ( [ 0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08,
              0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08
            ],
            [ 0xCA, 0xFE, 0xBA, 0xBE, 0xFA, 0xCE, 0xDB, 0xAD,
              0xDE, 0xCA, 0xF8, 0x88
            ],
            [ 0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xAB, 0xAD, 0xDA, 0xD2
            ],
            [ 0xD9, 0x31, 0x32, 0x25, 0xF8, 0x84, 0x06, 0xE5,
              0xA5, 0x59, 0x09, 0xC5, 0xAF, 0xF5, 0x26, 0x9A,
              0x86, 0xA7, 0xA9, 0x53, 0x15, 0x34, 0xF7, 0xDA,
              0x2E, 0x4C, 0x30, 0x3D, 0x8A, 0x31, 0x8A, 0x72,
              0x1C, 0x3C, 0x0C, 0x95, 0x95, 0x68, 0x09, 0x53,
              0x2F, 0xCF, 0x0E, 0x24, 0x49, 0xA6, 0xB5, 0x25,
              0xB1, 0x6A, 0xED, 0xF5, 0xAA, 0x0D, 0xE6, 0x57,
              0xBA, 0x63, 0x7B, 0x39
            ],
            ( [ 0x52, 0x2D, 0xC1, 0xF0, 0x99, 0x56, 0x7D, 0x07,
                0xF4, 0x7F, 0x37, 0xA3, 0x2A, 0x84, 0x42, 0x7D,
                0x64, 0x3A, 0x8C, 0xDC, 0xBF, 0xE5, 0xC0, 0xC9,
                0x75, 0x98, 0xA2, 0xBD, 0x25, 0x55, 0xD1, 0xAA,
                0x8C, 0xB0, 0x8E, 0x48, 0x59, 0x0D, 0xBB, 0x3D,
                0xA7, 0xB0, 0x8B, 0x10, 0x56, 0x82, 0x88, 0x38,
                0xC5, 0xF6, 0x1E, 0x63, 0x93, 0xBA, 0x7A, 0x0A,
                0xBC, 0xC9, 0xF6, 0x62
              ],
              [ 0x76, 0xFC, 0x6E, 0xCE, 0x0F, 0x4E, 0x17, 0x68,
                0xCD, 0xDF, 0x88, 0x53, 0xBB, 0x2D, 0x55, 0x1B
              ]
            )
          ),
          ( [ 0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08,
              0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08
            ],
            [ 0xCA, 0xFE, 0xBA, 0xBE, 0xFA, 0xCE, 0xDB, 0xAD
            ],
            [ 0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xAB, 0xAD, 0xDA, 0xD2
            ],
            [ 0xD9, 0x31, 0x32, 0x25, 0xF8, 0x84, 0x06, 0xE5,
              0xA5, 0x59, 0x09, 0xC5, 0xAF, 0xF5, 0x26, 0x9A,
              0x86, 0xA7, 0xA9, 0x53, 0x15, 0x34, 0xF7, 0xDA,
              0x2E, 0x4C, 0x30, 0x3D, 0x8A, 0x31, 0x8A, 0x72,
              0x1C, 0x3C, 0x0C, 0x95, 0x95, 0x68, 0x09, 0x53,
              0x2F, 0xCF, 0x0E, 0x24, 0x49, 0xA6, 0xB5, 0x25,
              0xB1, 0x6A, 0xED, 0xF5, 0xAA, 0x0D, 0xE6, 0x57,
              0xBA, 0x63, 0x7B, 0x39
            ],
            ( [ 0xC3, 0x76, 0x2D, 0xF1, 0xCA, 0x78, 0x7D, 0x32,
                0xAE, 0x47, 0xC1, 0x3B, 0xF1, 0x98, 0x44, 0xCB,
                0xAF, 0x1A, 0xE1, 0x4D, 0x0B, 0x97, 0x6A, 0xFA,
                0xC5, 0x2F, 0xF7, 0xD7, 0x9B, 0xBA, 0x9D, 0xE0,
                0xFE, 0xB5, 0x82, 0xD3, 0x39, 0x34, 0xA4, 0xF0,
                0x95, 0x4C, 0xC2, 0x36, 0x3B, 0xC7, 0x3F, 0x78,
                0x62, 0xAC, 0x43, 0x0E, 0x64, 0xAB, 0xE4, 0x99,
                0xF4, 0x7C, 0x9B, 0x1F
              ],
              [ 0x3A, 0x33, 0x7D, 0xBF, 0x46, 0xA7, 0x92, 0xC4,
                0x5E, 0x45, 0x49, 0x13, 0xFE, 0x2E, 0xA8, 0xF2
              ]
            )
          ),
          ( [ 0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08,
              0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08
            ],
            [ 0x93, 0x13, 0x22, 0x5D, 0xF8, 0x84, 0x06, 0xE5,
              0x55, 0x90, 0x9C, 0x5A, 0xFF, 0x52, 0x69, 0xAA,
              0x6A, 0x7A, 0x95, 0x38, 0x53, 0x4F, 0x7D, 0xA1,
              0xE4, 0xC3, 0x03, 0xD2, 0xA3, 0x18, 0xA7, 0x28,
              0xC3, 0xC0, 0xC9, 0x51, 0x56, 0x80, 0x95, 0x39,
              0xFC, 0xF0, 0xE2, 0x42, 0x9A, 0x6B, 0x52, 0x54,
              0x16, 0xAE, 0xDB, 0xF5, 0xA0, 0xDE, 0x6A, 0x57,
              0xA6, 0x37, 0xB3, 0x9B
            ],
            [ 0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xAB, 0xAD, 0xDA, 0xD2
            ],
            [ 0xD9, 0x31, 0x32, 0x25, 0xF8, 0x84, 0x06, 0xE5,
              0xA5, 0x59, 0x09, 0xC5, 0xAF, 0xF5, 0x26, 0x9A,
              0x86, 0xA7, 0xA9, 0x53, 0x15, 0x34, 0xF7, 0xDA,
              0x2E, 0x4C, 0x30, 0x3D, 0x8A, 0x31, 0x8A, 0x72,
              0x1C, 0x3C, 0x0C, 0x95, 0x95, 0x68, 0x09, 0x53,
              0x2F, 0xCF, 0x0E, 0x24, 0x49, 0xA6, 0xB5, 0x25,
              0xB1, 0x6A, 0xED, 0xF5, 0xAA, 0x0D, 0xE6, 0x57,
              0xBA, 0x63, 0x7B, 0x39
            ],
            ( [ 0x5A, 0x8D, 0xEF, 0x2F, 0x0C, 0x9E, 0x53, 0xF1,
                0xF7, 0x5D, 0x78, 0x53, 0x65, 0x9E, 0x2A, 0x20,
                0xEE, 0xB2, 0xB2, 0x2A, 0xAF, 0xDE, 0x64, 0x19,
                0xA0, 0x58, 0xAB, 0x4F, 0x6F, 0x74, 0x6B, 0xF4,
                0x0F, 0xC0, 0xC3, 0xB7, 0x80, 0xF2, 0x44, 0x45,
                0x2D, 0xA3, 0xEB, 0xF1, 0xC5, 0xD8, 0x2C, 0xDE,
                0xA2, 0x41, 0x89, 0x97, 0x20, 0x0E, 0xF8, 0x2E,
                0x44, 0xAE, 0x7E, 0x3F
              ],
              [ 0xA4, 0x4A, 0x82, 0x66, 0xEE, 0x1C, 0x8E, 0xB0,
                0xC8, 0xB5, 0xD4, 0xCF, 0x5A, 0xE9, 0xF1, 0x9A
              ]
            )
          )
        ]
      negativeTests =
        [ ( [ 0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08,
              0xFE, 0xFF, 0xE9, 0x92, 0x86, 0x65, 0x73, 0x1C,
              0x6D, 0x6A, 0x8F, 0x94, 0x67, 0x30, 0x83, 0x08
            ],
            [ 0xCA, 0xFE, 0xBA, 0xBE, 0xFA, 0xCE, 0xDB, 0xAD,
              0xDE, 0xCA, 0xF8, 0x88
            ],
            [ 0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xFE, 0xED, 0xFA, 0xCE, 0xDE, 0xAD, 0xBE, 0xEF,
              0xAB, 0xAD, 0xDA, 0xD2
            ],
            [ 0xD9, 0x31, 0x32, 0x25, 0xF8, 0x84, 0x06, 0xE5,
              0xA5, 0x59, 0x09, 0xC5, 0xAF, 0xF5, 0x26, 0x9A,
              0x86, 0xA7, 0xA9, 0x53, 0x15, 0x34, 0xF7, 0xDA,
              0x2E, 0x4C, 0x30, 0x3D, 0x8A, 0x31, 0x8A, 0x72,
              0x1C, 0x3C, 0x0C, 0x95, 0x95, 0x68, 0x09, 0x53,
              0x2F, 0xCF, 0x0E, 0x24, 0x49, 0xA6, 0xB5, 0x25,
              0xB1, 0x6A, 0xED, 0xF5, 0xAA, 0x0D, 0xE6, 0x57,
              0xBA, 0x63, 0x7B, 0x39
            ],
            ( [ 0x52, 0x2D, 0xC1, 0xF0, 0x99, 0x56, 0x7D, 0x07,
                0xF4, 0x7F, 0x37, 0xA3, 0x2A, 0x84, 0x42, 0x7D,
                0x64, 0x3A, 0x8C, 0xDC, 0xBF, 0xE5, 0xC0, 0xC9,
                0x75, 0x98, 0xA2, 0xBD, 0x25, 0x55, 0xD1, 0xAA,
                0x8C, 0xB0, 0x8E, 0x48, 0x59, 0x0D, 0xBB, 0x3D,
                0xA7, 0xB0, 0x8B, 0x10, 0x56, 0x82, 0x88, 0x38,
                0xC5, 0xF6, 0x1E, 0x63, 0x93, 0xBA, 0x7A, 0x0A,
                0xBC, 0xC9, 0xF6, 0x63
              ],
              [ 0x76, 0xFC, 0x6E, 0xCE, 0x0F, 0x4E, 0x17, 0x68,
                0xCD, 0xDF, 0x88, 0x53, 0xBB, 0x2D, 0x55, 0x1B
              ]
            )
          )
        ]
   in testsuite
        "AES256GCM"
        [ indexedTest
            "Randall2011Enc"
            $ fmap
              (\(key, iv, auth, ptext, ct) ->
                expectVarEq "ct" ct $ GCM.encrypt (AES256.encrypt key) iv auth ptext)
              randallTests,
          indexedTest
            "Randall2011Dec"
            $ fmap
              (\(key, iv, auth, ptext, ct) ->
                expectVarEq "ptext" (Right ptext) $ GCM.decrypt (AES256.encrypt key) iv auth ct)
              randallTests,
          indexedTest
            "MV2005Enc"
            $ fmap
              (\(key, iv, auth, ptext, ct) ->
                expectVarEq "ct" ct $ GCM.encrypt (AES256.encrypt key) iv auth ptext)
              mv2005Tests,
          indexedTest
            "MV2005Dec"
            $ fmap
              (\(key, iv, auth, ptext, ct) ->
                expectVarEq "ptext" (Right ptext) $ GCM.decrypt (AES256.encrypt key) iv auth ct)
              mv2005Tests,
          indexedTest
            "NegativeTests"
            $ fmap
              (\(key, iv, auth, ptext, ct) ->
                expectFalse . isRight $ GCM.decrypt (AES256.encrypt key) iv auth ct)
              negativeTests
        ]
