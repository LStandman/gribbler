-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Diceware/EFFWordlistTest.hs: Unit tests for the EFF large wordlist.
-- https://www.eff.org/files/2016/07/18/eff_large_wordlist.txt
-- Copyright (C) 2023 LStandman

module Misc.Diceware.EFFWordlistTest(test_effwordlist) where

import qualified Misc.Diceware as Diceware
import qualified Misc.Diceware.EFFWordlist as EFFWordlist
import Libtest

test_effwordlist =
  let
    t1_result = Right ()
    t2_result = 7776
  in
    testsuite "EFFWordlist" [
      test "IsSanitized" [
        expect_memeq "t1_result" t1_result $ Diceware.is_sanitized EFFWordlist.eff_large_wordlist],
      test "CorrectLength" [
        expect_memeq "t2_result" t2_result $ length EFFWordlist.eff_large_wordlist]]
