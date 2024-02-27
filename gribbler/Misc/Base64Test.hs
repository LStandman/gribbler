-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64Test.hs: Unit tests for Base64
-- Copyright (C) 2024 LStandman

module Misc.Base64Test(test_base64) where

import qualified Misc.Base64.RFC4648 as Base64
import Misc.MemUtils
import Libtest

test_base64 =
  let
    t01_plain    = strBytes ""
    t01_encoded  = ""
    t02_plain    = strBytes "f"
    t02_padded   = "Zg=="
    t02_unpadded = "Zg"
    t03_plain    = strBytes "fo"
    t03_padded   = "Zm8="
    t03_unpadded = "Zm8"
    t04_plain    = strBytes "foo"
    t04_encoded  = "Zm9v"
    t05_plain    = strBytes "foob"
    t05_padded   = "Zm9vYg=="
    t05_unpadded = "Zm9vYg"
    t06_plain    = strBytes "fooba"
    t06_padded   = "Zm9vYmE="
    t06_unpadded = "Zm9vYmE"
    t07_plain    = strBytes "foobar"
    t07_encoded  = "Zm9vYmFy"
  in
    testsuite "Base64" [
      test "EncodeRFC4648" [
        expect_memeq "t01_encoded"  t01_encoded  $ Base64.encode True  t01_plain,
        expect_memeq "t01_encoded"  t01_encoded  $ Base64.encode False t01_plain,
        expect_memeq "t02_padded"   t02_padded   $ Base64.encode True  t02_plain,
        expect_memeq "t02_unpadded" t02_unpadded $ Base64.encode False t02_plain,
        expect_memeq "t03_padded"   t03_padded   $ Base64.encode True  t03_plain,
        expect_memeq "t03_unpadded" t03_unpadded $ Base64.encode False t03_plain,
        expect_memeq "t04_encoded"  t04_encoded  $ Base64.encode True  t04_plain,
        expect_memeq "t04_encoded"  t04_encoded  $ Base64.encode False t04_plain,
        expect_memeq "t05_padded"   t05_padded   $ Base64.encode True  t05_plain,
        expect_memeq "t05_unpadded" t05_unpadded $ Base64.encode False t05_plain,
        expect_memeq "t06_padded"   t06_padded   $ Base64.encode True  t06_plain,
        expect_memeq "t06_unpadded" t06_unpadded $ Base64.encode False t06_plain,
        expect_memeq "t07_encoded"  t07_encoded  $ Base64.encode True  t07_plain,
        expect_memeq "t07_encoded"  t07_encoded  $ Base64.encode False t07_plain]]
