-- SPDX-License-Identifier: GPL-3.0-or-later
-- MiscTest.hs: Unit tests for misc module.
-- Copyright (C) 2023-2024 LStandman

module MiscTest (test_misc) where

import Libtest
import Misc.Base64Test
import Misc.Diceware.EFFWordlistTest
import Misc.DicewareTest
import Misc.DiffListTest

test_misc =
  runtests
    [ test_base64,
      test_diceware,
      test_difflist,
      test_effwordlist
    ]
