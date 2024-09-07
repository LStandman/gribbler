-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Base64Test.hs: Unit tests for Base64
-- Copyright (C) 2024 LStandman

module Misc.Base64Test (test_base64) where

import Data.Either
import Libtest
import qualified Misc.Base64.RFC4648 as Base64
import Misc.MemUtils

test_base64 =
  let t01_plain = strBytes ""
      t01_encoded = ""
      t02_plain = strBytes "f"
      t02_padded = "Zg=="
      t02_unpadded = "Zg"
      t03_plain = strBytes "fo"
      t03_padded = "Zm8="
      t03_unpadded = "Zm8"
      t04_plain = strBytes "foo"
      t04_encoded = "Zm9v"
      t05_plain = strBytes "foob"
      t05_padded = "Zm9vYg=="
      t05_unpadded = "Zm9vYg"
      t06_plain = strBytes "fooba"
      t06_padded = "Zm9vYmE="
      t06_unpadded = "Zm9vYmE"
      t07_plain = strBytes "foobar"
      t07_encoded = "Zm9vYmFy"
   in testsuite
        "Base64"
        [ test
            "EncodeRFC4648"
            [ expectMemEq "t01_encoded" t01_encoded $ Base64.encode True t01_plain,
              expectMemEq "t01_encoded" t01_encoded $ Base64.encode False t01_plain,
              expectMemEq "t02_padded" t02_padded $ Base64.encode True t02_plain,
              expectMemEq "t02_unpadded" t02_unpadded $ Base64.encode False t02_plain,
              expectMemEq "t03_padded" t03_padded $ Base64.encode True t03_plain,
              expectMemEq "t03_unpadded" t03_unpadded $ Base64.encode False t03_plain,
              expectMemEq "t04_encoded" t04_encoded $ Base64.encode True t04_plain,
              expectMemEq "t04_encoded" t04_encoded $ Base64.encode False t04_plain,
              expectMemEq "t05_padded" t05_padded $ Base64.encode True t05_plain,
              expectMemEq "t05_unpadded" t05_unpadded $ Base64.encode False t05_plain,
              expectMemEq "t06_padded" t06_padded $ Base64.encode True t06_plain,
              expectMemEq "t06_unpadded" t06_unpadded $ Base64.encode False t06_plain,
              expectMemEq "t07_encoded" t07_encoded $ Base64.encode True t07_plain,
              expectMemEq "t07_encoded" t07_encoded $ Base64.encode False t07_plain
            ],
          test
            "DecodeRFC4648"
            [ expectMemEq "t01_plain" (Right t01_plain) $ Base64.decode t01_encoded,
              expectMemEq "t02_plain" (Right t02_plain) $ Base64.decode t02_padded,
              expectMemEq "t02_plain" (Right t02_plain) $ Base64.decode t02_unpadded,
              expectMemEq "t03_plain" (Right t03_plain) $ Base64.decode t03_padded,
              expectMemEq "t03_plain" (Right t03_plain) $ Base64.decode t03_unpadded,
              expectMemEq "t04_plain" (Right t04_plain) $ Base64.decode t04_encoded,
              expectMemEq "t05_plain" (Right t05_plain) $ Base64.decode t05_padded,
              expectMemEq "t05_plain" (Right t05_plain) $ Base64.decode t05_unpadded,
              expectMemEq "t06_plain" (Right t06_plain) $ Base64.decode t06_padded,
              expectMemEq "t06_plain" (Right t06_plain) $ Base64.decode t06_unpadded,
              expectMemEq "t07_plain" (Right t07_plain) $ Base64.decode t07_encoded
            ]
        ]
