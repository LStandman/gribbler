-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNFTest.hs: Unit tests for BNF
-- Copyright (C) 2022-2023 LStandman

module BNFTest(test_bnf) where

import Libtest
import BNF
import BNF.Text

test_bnf =
  let
    errorX       = Error "X" :: Result Text
    errorY       = Error "Y" :: Result Text
    amiss        = Miss :: Result Text
    errX         = return errorX
    errY         = return errorY
    miss         = return amiss
    t1_in        = text ""
    t1_successA  = Hit ("", "a")
    t1_successB  = Hit ("", "b")
    t1_hitA      = return t1_successA
    t1_hitB      = return t1_successB
    t1_hitxhit   = t1_successA
    t1_hitxmiss  = t1_successA
    t1_hitxerr   = t1_successA
    t1_missxhit  = t1_successB
    t1_missxmiss = amiss
    t1_missxerr  = errorY
    t1_errxhit   = errorX
    t1_errxmiss  = errorX
    t1_errxerr   = errorX
    t2_in        = text "ab"
    t2_success   = Hit ("", "ab")
    t2_hitA      = match_char 'a'
    t2_hitB      = match_char 'b'
    t2_hitxhit   = t2_success
    t2_hitxmiss  = amiss
    t2_hitxerr   = errorY
    t2_missxhit  = amiss
    t2_missxmiss = amiss
    t2_missxerr  = amiss
    t2_errxhit   = errorX
    t2_errxmiss  = errorX
    t2_errxerr   = errorX
    t3_in        = text "a"
    t3_success   = Hit ("", "a")
    t3_hitA      = match_char 'a'
    t3_hitB      = match_char 'a'
    t3_hitxhit   = amiss
    t3_hitxmiss  = t3_success
    t3_hitxerr   = errorY
    t3_missxhit  = amiss
    t3_missxmiss = amiss
    t3_missxerr  = amiss
    t3_errxhit   = errorX
    t3_errxmiss  = errorX
    t3_errxerr   = errorX
    t4_map       = \ (x, y) -> (x, length y)
    t4_ihit      = Hit ("xyz", "abc")
    t4_ohit      = Hit ("xyz", 3)
    t4_imiss     = Miss :: Result Text
    t4_omiss     = Miss :: Result (String, Int)
    t4_ierr      = Error "X" :: Result Text
    t4_oerr      = Error "X" :: Result (String, Int)
    t5_hit       = match_char 'a'
    t5_reps      = 2
    t5_inA       = text "abb"
    t5_outA      = Miss
    t5_inB       = text "aab"
    t5_outB      = Hit ("b", "aa")
    t5_inC       = text "aaa"
    t5_outC      = Hit ("a", "aa")
    t6_hit       = match_char 'a'
    t6_inA       = text "aa"
    t6_outA      = Hit ("a", "a")
    t6_inB       = text "ba"
    t6_outB      = Hit ("ba", "")
    t7_hit       = match_char 'a'
    t7_inA       = text "bbb"
    t7_outA      = Hit ("bbb", "")
    t7_inB       = text "abb"
    t7_outB      = Hit ("bb", "a")
    t7_inC       = text "aab"
    t7_outC      = Hit ("b", "aa")
    t8_hit       = match_char 'a'
    t8_inA       = text "bbb"
    t8_outA      = Miss
    t8_inB       = text "abb"
    t8_outB      = Hit ("bb", "a")
    t8_inC       = text "aab"
    t8_outC      = Hit ("b", "aa")
  in
    testsuite "BNF" [
      test "Ou" [
        expect_memeq "t1_hitxhit" t1_hitxhit $
        (t1_hitA `BNF.ou` t1_hitB) t1_in,
        expect_memeq "t1_hitxmiss" t1_hitxmiss $
        (t1_hitA `BNF.ou` miss) t1_in,
        expect_memeq "t1_hitxerr" t1_hitxerr $
        (t1_hitA `BNF.ou` errY) t1_in,
        expect_memeq "t1_missxhit" t1_missxhit $
        (miss `BNF.ou` t1_hitB) t1_in,
        expect_memeq "t1_missxmiss" t1_missxmiss $
        (miss `BNF.ou` miss) t1_in,
        expect_memeq "t1_missxerr" t1_missxerr $
        (miss `BNF.ou` errY) t1_in,
        expect_memeq "t1_errxhit" t1_errxhit $
        (errX `BNF.ou` t1_hitB) t1_in,
        expect_memeq "t1_errxmiss" t1_errxmiss $
        (errX `BNF.ou` miss) t1_in,
        expect_memeq "t1_errxerr" t1_errxerr $
        (errX `BNF.ou` errY) t1_in],
      test "Et" [
        expect_memeq "t2_hitxhit" t2_hitxhit $
        (t2_hitA `BNF.et` t2_hitB) t2_in,
        expect_memeq "t2_hitxmiss" t2_hitxmiss $
        (t2_hitA `BNF.et` miss) t2_in,
        expect_memeq "t2_hitxerr" t2_hitxerr $
        (t2_hitA `BNF.et` errY) t2_in,
        expect_memeq "t2_missxhit" t2_missxhit $
        (miss `BNF.et` t2_hitB) t2_in,
        expect_memeq "t2_missxmiss" t2_missxmiss $
        (miss `BNF.et` miss) t2_in,
        expect_memeq "t2_missxerr" t2_missxerr $
        (miss `BNF.et` errY) t2_in,
        expect_memeq "t2_errxhit" t2_errxhit $
        (errX `BNF.et` t2_hitB) t2_in,
        expect_memeq "t2_errxmiss" t2_errxmiss $
        (errX `BNF.et` miss) t2_in,
        expect_memeq "t2_errxerr" t2_errxerr $
        (errX `BNF.et` errY) t2_in],
      test "Sauf" [
        expect_memeq "t3_hitxhit" t3_hitxhit $
        (t3_hitA `BNF.sauf` t3_hitB) t3_in,
        expect_memeq "t3_hitxmiss" t3_hitxmiss $
        (t3_hitA `BNF.sauf` miss) t3_in,
        expect_memeq "t3_hitxerr" t3_hitxerr $
        (t3_hitA `BNF.sauf` errY) t3_in,
        expect_memeq "t3_missxhit" t3_missxhit $
        (miss `BNF.sauf` t3_hitB) t3_in,
        expect_memeq "t3_missxmiss" t3_missxmiss $
        (miss `BNF.sauf` miss) t3_in,
        expect_memeq "t3_missxerr" t3_missxerr $
        (miss `BNF.sauf` errY) t3_in,
        expect_memeq "t3_errxhit" t3_errxhit $
        (errX `BNF.sauf` t3_hitB) t3_in,
        expect_memeq "t3_errxmiss" t3_errxmiss $
        (errX `BNF.sauf` miss) t3_in,
        expect_memeq "t3_errxerr" t3_errxerr $
        (errX `BNF.sauf` errY) t3_in],
      test "Fmap" [
        expect_memeq "t4_ohit" t4_ohit $
        fmap (t4_map) t4_ihit,
        expect_memeq "t4_omiss" t4_omiss $
        fmap (t4_map) t4_imiss,
        expect_memeq "t4_oerr" t4_oerr $
        fmap (t4_map) t4_ierr],
      test "Rep" [
        expect_memeq "t5_outA" t5_outA $
        (BNF.rep t5_reps t5_hit) t5_inA,
        expect_memeq "t5_outB" t5_outB $
        (BNF.rep t5_reps t5_hit) t5_inB,
        expect_memeq "t5_outC" t5_outC $
        (BNF.rep t5_reps t5_hit) t5_inC],
      test "ZeroOrOne" [
        expect_memeq "t6_outA" t6_outA $
        (BNF.zero_or_one t6_hit) t6_inA,
        expect_memeq "t6_outB" t6_outB $
        (BNF.zero_or_one t6_hit) t6_inB],
      test "ZeroOrMore" [
        expect_memeq "t7_outA" t7_outA $
        (BNF.zero_or_more t7_hit) t7_inA,
        expect_memeq "t7_outB" t7_outB $
        (BNF.zero_or_more t7_hit) t7_inB,
        expect_memeq "t7_outC" t7_outC $
        (BNF.zero_or_more t7_hit) t7_inC],
      test "OneOrMore" [
        expect_memeq "t8_outA" t8_outA $
        (BNF.one_or_more t8_hit) t8_inA,
        expect_memeq "t8_outB" t8_outB $
        (BNF.one_or_more t8_hit) t8_inB,
        expect_memeq "t8_outC" t8_outC $
        (BNF.one_or_more t8_hit) t8_inC]]
