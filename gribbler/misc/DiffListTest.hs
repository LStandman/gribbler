-- SPDX-License-Identifier: GPL-3.0-or-later
-- DiffListTest.hs: Unit tests for DiffList
-- Copyright (C) 2023 LStandman

module DiffListTest(test_difflist) where

import DiffList
import Libtest

test_difflist =
  let
    t1_listA  = [ '1', '2', '3', '4' ]
    t1_listB  = [ 'a', 'b', 'c', 'd' ]
    t1_result = [ '1', '2', '3', '4', 'a', 'b', 'c', 'd' ]
  in
    testsuite "DiffList" [
      test "Concat" [
        expect_memeq "t1_result" t1_result $ relist $ 
          (difflist t1_listA) <> (difflist t1_listB)]]
