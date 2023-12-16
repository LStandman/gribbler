-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNFTest.hs: Unit tests for BNF
-- Copyright (C) 2022-2023 LStandman

module BNFTest(test_bnf) where

import Data.Char
--
import Libtest
import qualified BNF as BNF
import BNF.Text

test_bnf =
  let
    errorX        = BNF.Error "X" :: BNF.Result (DiffString, String)

    errorY        = BNF.Error "Y" :: BNF.Result (DiffString, String)

    amiss         = BNF.Miss :: BNF.Result (DiffString, String)

    errX          = BNF.Parser (\ _ -> errorX)
    errY          = BNF.Parser (\ _ -> errorY)
    miss          = BNF.Parser (\ _ -> amiss)
    t1_in         = ""
    t1_successA   = BNF.Hit (difflist "a", "")
    t1_successB   = BNF.Hit (difflist "b", "")
    t1_hitA       = BNF.Parser (\ _ -> t1_successA)
    t1_hitB       = BNF.Parser (\ _ -> t1_successB)
    t1_hitxhit    = t1_successA
    t1_hitxmiss   = t1_successA
    t1_hitxerr    = t1_successA
    t1_missxhit   = t1_successB
    t1_missxmiss  = amiss
    t1_missxerr   = errorY
    t1_errxhit    = errorX
    t1_errxmiss   = errorX
    t1_errxerr    = errorX
    t2_in         = "ab"
    t2_success    = BNF.Hit (difflist "ab", "")
    t2_hitA       = get_char 'a'
    t2_hitB       = get_char 'b'
    t2_hitxhit    = t2_success
    t2_hitxmiss   = amiss
    t2_hitxerr    = errorY
    t2_missxhit   = amiss
    t2_missxmiss  = amiss
    t2_missxerr   = amiss
    t2_errxhit    = errorX
    t2_errxmiss   = errorX
    t2_errxerr    = errorX
    t3_in         = "a"
    t3_success    = BNF.Hit (difflist "a", "")
    t3_hitA       = get_char 'a'
    t3_hitB       = get_char 'a'
    t3_hitxhit    = amiss
    t3_hitxmiss   = t3_success
    t3_hitxerr    = errorY
    t3_missxhit   = amiss
    t3_missxmiss  = amiss
    t3_missxerr   = amiss
    t3_errxhit    = errorX
    t3_errxmiss   = errorX
    t3_errxerr    = errorX
    t4_hit        = get_char 'a'
    t4_reps       = 2
    t4_inA        = "abb"
    t4_outA       = BNF.Miss
    t4_inB        = "aab"
    t4_outB       = BNF.Hit (difflist "aa", "b")
    t4_inC        = "aaa"
    t4_outC       = BNF.Hit (difflist "aa", "a")
    t5_hit        = get_char 'a'
    t5_inA        = "aa"
    t5_outA       = BNF.Hit (difflist "a", "a")
    t5_inB        = "ba"
    t5_outB       = BNF.Hit (difflist "", "ba")
    t6_hit        = get_char 'a'
    t6_inA        = "bbb"
    t6_outA       = BNF.Hit (difflist "", "bbb")
    t6_inB        = "abb"
    t6_outB       = BNF.Hit (difflist "a", "bb")
    t6_inC        = "aab"
    t6_outC       = BNF.Hit (difflist "aa", "b")
    t7_hit        = get_char 'a'
    t7_inA        = "bbb"
    t7_outA       = BNF.Miss
    t7_inB        = "abb"
    t7_outB       = BNF.Hit (difflist "a", "bb")
    t7_inC        = "aab"
    t7_outC       = BNF.Hit (difflist "aa", "b")
    t8_in        = ""
    t8_errStr    = "Y"
    t8_errA      = BNF.Error "X"
    t8_errB      = BNF.Error t8_errStr
    t8_fhit      = return $ BNF.Hit (difflist "a", "")
    t8_ohit      = t8_errB
    t8_fmiss     = BNF.Parser (\ _ -> BNF.Miss) :: TextParser
    t8_omiss     = BNF.Miss
    t8_ferr      = BNF.Parser (\ _ -> t8_errA) :: TextParser
    t8_oerr      = t8_errA
  in
    testsuite "BNF" [
      test "Or" [
        expect_memeq "t1_hitxhit" t1_hitxhit $
        BNF.run_parser (t1_hitA `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_hitxmiss" t1_hitxmiss $
        BNF.run_parser (t1_hitA `BNF.or` miss) t1_in,
        expect_memeq "t1_hitxerr" t1_hitxerr $
        BNF.run_parser (t1_hitA `BNF.or` errY) t1_in,
        expect_memeq "t1_missxhit" t1_missxhit $
        BNF.run_parser (miss `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_missxmiss" t1_missxmiss $
        BNF.run_parser (miss `BNF.or` miss) t1_in,
        expect_memeq "t1_missxerr" t1_missxerr $
        BNF.run_parser (miss `BNF.or` errY) t1_in,
        expect_memeq "t1_errxhit" t1_errxhit $
        BNF.run_parser (errX `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_errxmiss" t1_errxmiss $
        BNF.run_parser (errX `BNF.or` miss) t1_in,
        expect_memeq "t1_errxerr" t1_errxerr $
        BNF.run_parser (errX `BNF.or` errY) t1_in],
      test "And" [
        expect_memeq "t2_hitxhit" t2_hitxhit $
        BNF.run_parser (t2_hitA `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_hitxmiss" t2_hitxmiss $
        BNF.run_parser (t2_hitA `BNF.and` miss) t2_in,
        expect_memeq "t2_hitxerr" t2_hitxerr $
        BNF.run_parser (t2_hitA `BNF.and` errY) t2_in,
        expect_memeq "t2_missxhit" t2_missxhit $
        BNF.run_parser (miss `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_missxmiss" t2_missxmiss $
        BNF.run_parser (miss `BNF.and` miss) t2_in,
        expect_memeq "t2_missxerr" t2_missxerr $
        BNF.run_parser (miss `BNF.and` errY) t2_in,
        expect_memeq "t2_errxhit" t2_errxhit $
        BNF.run_parser (errX `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_errxmiss" t2_errxmiss $
        BNF.run_parser (errX `BNF.and` miss) t2_in,
        expect_memeq "t2_errxerr" t2_errxerr $
        BNF.run_parser (errX `BNF.and` errY) t2_in],
      test "Except" [
        expect_memeq "t3_hitxhit" t3_hitxhit $
        BNF.run_parser (t3_hitA `BNF.except` t3_hitB) t3_in,
        expect_memeq "t3_hitxmiss" t3_hitxmiss $
        BNF.run_parser (t3_hitA `BNF.except` miss) t3_in,
        expect_memeq "t3_hitxerr" t3_hitxerr $
        BNF.run_parser (t3_hitA `BNF.except` errY) t3_in,
        expect_memeq "t3_missxhit" t3_missxhit $
        BNF.run_parser (miss `BNF.except` t3_hitB) t3_in,
        expect_memeq "t3_missxmiss" t3_missxmiss $
        BNF.run_parser (miss `BNF.except` miss) t3_in,
        expect_memeq "t3_missxerr" t3_missxerr $
        BNF.run_parser (miss `BNF.except` errY) t3_in,
        expect_memeq "t3_errxhit" t3_errxhit $
        BNF.run_parser (errX `BNF.except` t3_hitB) t3_in,
        expect_memeq "t3_errxmiss" t3_errxmiss $
        BNF.run_parser (errX `BNF.except` miss) t3_in,
        expect_memeq "t3_errxerr" t3_errxerr $
        BNF.run_parser (errX `BNF.except` errY) t3_in],
      test "Rep" [
        expect_memeq "t4_outA" t4_outA $
        BNF.run_parser (BNF.rep t4_reps t4_hit) t4_inA,
        expect_memeq "t4_outB" t4_outB $
        BNF.run_parser (BNF.rep t4_reps t4_hit) t4_inB,
        expect_memeq "t4_outC" t4_outC $
        BNF.run_parser (BNF.rep t4_reps t4_hit) t4_inC],
      test "ZOO" [
        expect_memeq "t5_outA" t5_outA $
        BNF.run_parser (BNF.zoo t5_hit) t5_inA,
        expect_memeq "t5_outB" t5_outB $
        BNF.run_parser (BNF.zoo t5_hit) t5_inB],
      test "ZOM" [
        expect_memeq "t6_outA" t6_outA $
        BNF.run_parser (BNF.zom t6_hit) t6_inA,
        expect_memeq "t6_outB" t6_outB $
        BNF.run_parser (BNF.zom t6_hit) t6_inB,
        expect_memeq "t6_outC" t6_outC $
        BNF.run_parser (BNF.zom t6_hit) t6_inC],
      test "OOM" [
        expect_memeq "t7_outA" t7_outA $
        BNF.run_parser (BNF.oom t7_hit) t7_inA,
        expect_memeq "t7_outB" t7_outB $
        BNF.run_parser (BNF.oom t7_hit) t7_inB,
        expect_memeq "t7_outC" t7_outC $
        BNF.run_parser (BNF.oom t7_hit) t7_inC],
      test "Err" [
        expect_memeq "t8_ohit" t8_ohit $
        BNF.run_parser (t8_fhit `BNF.err` t8_errStr) t8_in,
        expect_memeq "t8_omiss" t8_omiss $
        BNF.run_parser (t8_fmiss `BNF.err` t8_errStr) t8_in,
        expect_memeq "t8_oerr" t8_oerr $
        BNF.run_parser (t8_ferr `BNF.err` t8_errStr) t8_in]]
