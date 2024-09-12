-- SPDX-License-Identifier: GPL-3.0-or-later
-- CryptTest.hs: Unit tests for cryptographic module.
-- Copyright (C) 2023 LStandman

module CryptTest (test_crypt) where

import Crypt.AES256Test
import Crypt.Curve25519Test
import Crypt.KDFTest
import Crypt.MooMooTest
import Crypt.SHA256Test
import Crypt.SHA512Test
import Libtest

test_crypt =
  runtests
    [ testAes256,
      testAes256Cbc,
      testHkdfSha256,
      testHmacSha256,
      testPbkdf2HmacSha256,
      testSha256,
      testSha512,
      testX25519
    ]
