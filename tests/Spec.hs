-- SPDX-License-Identifier: GPL-3.0-or-later
-- Spec.hs: Unit test suite
-- Copyright (C) 2021-2023 LStandman

module Main where

--
import CryptTest
import JSONTest
import Libtest
import MiscTest
import System.Exit

main :: IO ()
main =
  runtests
    [ test_crypt,
      test_misc,
      test_json
    ] ()
    >>= \x -> if x then exitSuccess else exitFailure
