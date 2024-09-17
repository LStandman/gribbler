-- SPDX-License-Identifier: GPL-3.0-or-later
-- CryptTest.hs: Unit tests for cryptographic module.
-- Copyright (C) 2023 LStandman

module CryptTest (testCrypt) where

import Crypt.AES256Test
import Crypt.Curve25519Test
import Crypt.KDFTest
import Crypt.MooMooTest
import Crypt.SHA256Test
import Crypt.SHA512Test
import Libtest

testCrypt =
  runtests
    [ testAes256,
      testHkdfSha256,
      testHmacSha256,
      testPbkdf2HmacSha256,
      testMoo,
      testSha256,
      testSha512,
      testX25519
    ]
