-- SPDX-License-Identifier: GPL-3.0-or-later
-- Crypt/MooMooTest.hs: Unit tests for block cypher MOO.
-- Copyright (C) 2024 LStandman

module Crypt.MooMooTest (testMoo) where

import Crypt.MooMoo.CBCTest
import Libtest

testMoo =
  runtests
    [ testAes256Cbc
    ]
