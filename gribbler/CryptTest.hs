-- SPDX-License-Identifier: GPL-3.0-or-later
-- CryptTest.hs: Unit tests for cryptographic module.
-- Copyright (C) 2023 LStandman

module CryptTest(test_crypt) where

import Crypt.AES256Test
import Crypt.KDFTest
import Crypt.MooMooTest
import Crypt.SHA2Test
import Libtest

test_crypt =
  runtests [
    test_aes256,
    test_aes256_cbc,
    test_hmac_sha256,
    test_pbkdf2_hmac_sha256,
    test_sha256]
