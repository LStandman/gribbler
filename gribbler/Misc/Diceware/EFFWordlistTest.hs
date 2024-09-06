-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Diceware/EFFWordlistTest.hs: Unit tests for the EFF large wordlist.
-- https://www.eff.org/files/2016/07/18/eff_large_wordlist.txt
-- Copyright (C) 2023 LStandman

module Misc.Diceware.EFFWordlistTest (test_effwordlist) where

import Crypt.SHA2
import Libtest
import qualified Misc.Diceware as Diceware
import qualified Misc.Diceware.EFFWordlist as EFFWordlist
import Misc.MemUtils

{- ORMOLU_DISABLE -}
test_effwordlist =
  let t1_result = Right ()
      t2_result =
        [ 0x6d, 0x55, 0x7f, 0x06, 0x93, 0x95, 0x8f, 0xb5,
          0xe6, 0x50, 0xb6, 0x8b, 0x5b, 0xee, 0x58, 0x5e,
          0xb8, 0x2c, 0xf4, 0xda, 0x32, 0x96, 0x55, 0x05,
          0xc7, 0x89, 0xe9, 0x24, 0x74, 0x3b, 0xc5, 0x22
        ]
   in testsuite
        "EFFWordlist"
        [ test
            "IsSanitized"
            [ expect_memeq "t1_result" t1_result $
                Diceware.is_sanitized EFFWordlist.eff_large_wordlist
            ],
          test
            "SHA256"
            [ expect_memeq "t2_result" t2_result $
                sha256sum1 $ strBytes $ unlines EFFWordlist.eff_large_wordlist
            ]
        ]
