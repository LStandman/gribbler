-- SPDX-License-Identifier: GPL-3.0-or-later
-- MiscTest.hs: Unit tests for misc module.
-- Copyright (C) 2023-2024 LStandman

module MiscTest (testMisc) where

import Libtest
import Misc.Base64Test
import Misc.Diceware.EFFWordlistTest
import Misc.DicewareTest
import Misc.DiffListTest

testMisc =
  runtests
    [ testBase64,
      testDiceware,
      testDifflist,
      testEffwordlist
    ]
