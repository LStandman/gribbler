-- SPDX-License-Identifier: GPL-3.0-or-later
-- Spec.hs: Unit test suite
-- Copyright (C) 2021-2023 LStandman

module Main where

import System.Exit
--
import Crypt.AES256Test
import Crypt.KDFTest
import Crypt.MooMooTest
import Crypt.SHA2Test
import JSON.BNFTest
import Misc.DicewareTest
import Misc.DiffListTest

testmain :: [IO Bool] -> IO Bool
testmain [] = return True
testmain (t:ts) = t >>= \ x -> testmain ts >>= \ y -> return (x && y)

main :: IO ()
main =
  testmain [
    test_aes256,
    test_aes256_cbc,
    test_bnf,
    test_diceware,
    test_difflist,
    test_hmac_sha256,
    test_pbkdf2_hmac_sha256,
    test_sha256] >>=
  \ x -> if x then exitSuccess else exitFailure
