-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/DiffListTest.hs: Unit tests for DiffList
-- Copyright (C) 2023 LStandman

module Misc.DiffListTest (test_difflist) where

import Libtest
import Misc.DiffList

test_difflist =
  let t1_listA = ['1', '2', '3', '4']
      t1_listB = ['a', 'b', 'c', 'd']
      t1_result = ['1', '2', '3', '4', 'a', 'b', 'c', 'd']
   in testsuite
        "DiffList"
        [ test
            "Concat"
            [ expectMemEq "t1_result" t1_result $
                relist $
                  difflist t1_listA <> difflist t1_listB
            ]
        ]
