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
            [ expectVarEq "t01_encoded" t01_encoded $ Base64.encode True t01_plain,
              expectVarEq "t01_encoded" t01_encoded $ Base64.encode False t01_plain,
              expectVarEq "t02_padded" t02_padded $ Base64.encode True t02_plain,
              expectVarEq "t02_unpadded" t02_unpadded $ Base64.encode False t02_plain,
              expectVarEq "t03_padded" t03_padded $ Base64.encode True t03_plain,
              expectVarEq "t03_unpadded" t03_unpadded $ Base64.encode False t03_plain,
              expectVarEq "t04_encoded" t04_encoded $ Base64.encode True t04_plain,
              expectVarEq "t04_encoded" t04_encoded $ Base64.encode False t04_plain,
              expectVarEq "t05_padded" t05_padded $ Base64.encode True t05_plain,
              expectVarEq "t05_unpadded" t05_unpadded $ Base64.encode False t05_plain,
              expectVarEq "t06_padded" t06_padded $ Base64.encode True t06_plain,
              expectVarEq "t06_unpadded" t06_unpadded $ Base64.encode False t06_plain,
              expectVarEq "t07_encoded" t07_encoded $ Base64.encode True t07_plain,
              expectVarEq "t07_encoded" t07_encoded $ Base64.encode False t07_plain
            ],
          test
            "DecodeRFC4648"
            [ expectVarEq "t01_plain" (Right t01_plain) $ Base64.decode t01_encoded,
              expectVarEq "t02_plain" (Right t02_plain) $ Base64.decode t02_padded,
              expectVarEq "t02_plain" (Right t02_plain) $ Base64.decode t02_unpadded,
              expectVarEq "t03_plain" (Right t03_plain) $ Base64.decode t03_padded,
              expectVarEq "t03_plain" (Right t03_plain) $ Base64.decode t03_unpadded,
              expectVarEq "t04_plain" (Right t04_plain) $ Base64.decode t04_encoded,
              expectVarEq "t05_plain" (Right t05_plain) $ Base64.decode t05_padded,
              expectVarEq "t05_plain" (Right t05_plain) $ Base64.decode t05_unpadded,
              expectVarEq "t06_plain" (Right t06_plain) $ Base64.decode t06_padded,
              expectVarEq "t06_plain" (Right t06_plain) $ Base64.decode t06_unpadded,
              expectVarEq "t07_plain" (Right t07_plain) $ Base64.decode t07_encoded
            ]
        ]
