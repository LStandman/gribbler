-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNFTest.hs: Unit tests for BNF
-- Copyright (C) 2023 LStandman

module BNFTest(test_bnf) where

import Libtest
import BNF

match_char :: Char -> Production String
match_char c = \ (x:xs) -> case c == x of
  True  -> Hit [x] xs
  False -> Miss

test_bnf =
  let
    errorX       = Error "X" :: Resultant String
    errorY       = Error "Y" :: Resultant String
    amiss        = Miss :: Resultant String
    errX         = return errorX
    errY         = return errorY
    miss         = return amiss
    t1_in        = ""
    t1_successA  = Hit "a" ""
    t1_successB  = Hit "b" ""
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
    t2_in        = "ab"
    t2_success   = Hit "ab" ""
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
    t3_in        = "a"
    t3_success   = Hit "a" ""
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
    t4_map       = length
    t4_ihit      = Hit "abc" "xyz"
    t4_ohit      = Hit 3 "xyz"
    t4_imiss     = Miss :: Resultant String
    t4_omiss     = Miss :: Resultant Int
    t4_ierr      = Error "X" :: Resultant String
    t4_oerr      = Error "X" :: Resultant Int
    t5_in        = ""
    t5_map       = t4_map
    t5_ihit      = return t4_ihit
    t5_ohit      = t4_ohit
    t5_imiss     = return t4_imiss
    t5_omiss     = t4_omiss
    t5_ierr      = return t4_ierr
    t5_oerr      = t4_oerr
    t6_hit       = match_char 'a'
    t6_reps      = 2
    t6_inA       = "abb"
    t6_outA      = Miss
    t6_inB       = "aab"
    t6_outB      = Hit "aa" "b"
    t6_inC       = "aaa"
    t6_outC      = Hit "aa" "a"
    t7_hit       = match_char 'a'
    t7_inA       = "aa"
    t7_outA      = Hit "a" "a"
    t7_inB       = "ba"
    t7_outB      = Hit "" "ba"
    t8_hit       = match_char 'a'
    t8_inA       = "bbb"
    t8_outA      = Hit "" "bbb"
    t8_inB       = "abb"
    t8_outB      = Hit "a" "bb"
    t8_inC       = "aab"
    t8_outC      = Hit "aa" "b"
    t9_hit       = match_char 'a'
    t9_inA       = "bbb"
    t9_outA      = Miss
    t9_inB       = "abb"
    t9_outB      = Hit "a" "bb"
    t9_inC       = "aab"
    t9_outC      = Hit "aa" "b"
  in
    testsuite "BNF" [
      test "Altr" [
        expect_memeq "t1_hitxhit" t1_hitxhit $
        (t1_hitA `BNF.altr` t1_hitB) t1_in,
        expect_memeq "t1_hitxmiss" t1_hitxmiss $
        (t1_hitA `BNF.altr` miss) t1_in,
        expect_memeq "t1_hitxerr" t1_hitxerr $
        (t1_hitA `BNF.altr` errY) t1_in,
        expect_memeq "t1_missxhit" t1_missxhit $
        (miss `BNF.altr` t1_hitB) t1_in,
        expect_memeq "t1_missxmiss" t1_missxmiss $
        (miss `BNF.altr` miss) t1_in,
        expect_memeq "t1_missxerr" t1_missxerr $
        (miss `BNF.altr` errY) t1_in,
        expect_memeq "t1_errxhit" t1_errxhit $
        (errX `BNF.altr` t1_hitB) t1_in,
        expect_memeq "t1_errxmiss" t1_errxmiss $
        (errX `BNF.altr` miss) t1_in,
        expect_memeq "t1_errxerr" t1_errxerr $
        (errX `BNF.altr` errY) t1_in],
      test "Conc" [
        expect_memeq "t2_hitxhit" t2_hitxhit $
        (t2_hitA `BNF.conc` t2_hitB) t2_in,
        expect_memeq "t2_hitxmiss" t2_hitxmiss $
        (t2_hitA `BNF.conc` miss) t2_in,
        expect_memeq "t2_hitxerr" t2_hitxerr $
        (t2_hitA `BNF.conc` errY) t2_in,
        expect_memeq "t2_missxhit" t2_missxhit $
        (miss `BNF.conc` t2_hitB) t2_in,
        expect_memeq "t2_missxmiss" t2_missxmiss $
        (miss `BNF.conc` miss) t2_in,
        expect_memeq "t2_missxerr" t2_missxerr $
        (miss `BNF.conc` errY) t2_in,
        expect_memeq "t2_errxhit" t2_errxhit $
        (errX `BNF.conc` t2_hitB) t2_in,
        expect_memeq "t2_errxmiss" t2_errxmiss $
        (errX `BNF.conc` miss) t2_in,
        expect_memeq "t2_errxerr" t2_errxerr $
        (errX `BNF.conc` errY) t2_in],
      test "Exclude" [
        expect_memeq "t3_hitxhit" t3_hitxhit $
        (t3_hitA `BNF.exclude` t3_hitB) t3_in,
        expect_memeq "t3_hitxmiss" t3_hitxmiss $
        (t3_hitA `BNF.exclude` miss) t3_in,
        expect_memeq "t3_hitxerr" t3_hitxerr $
        (t3_hitA `BNF.exclude` errY) t3_in,
        expect_memeq "t3_missxhit" t3_missxhit $
        (miss `BNF.exclude` t3_hitB) t3_in,
        expect_memeq "t3_missxmiss" t3_missxmiss $
        (miss `BNF.exclude` miss) t3_in,
        expect_memeq "t3_missxerr" t3_missxerr $
        (miss `BNF.exclude` errY) t3_in,
        expect_memeq "t3_errxhit" t3_errxhit $
        (errX `BNF.exclude` t3_hitB) t3_in,
        expect_memeq "t3_errxmiss" t3_errxmiss $
        (errX `BNF.exclude` miss) t3_in,
        expect_memeq "t3_errxerr" t3_errxerr $
        (errX `BNF.exclude` errY) t3_in],
      test "Fmap" [
        expect_memeq "t4_ohit" t4_ohit $
        fmap (t4_map) t4_ihit,
        expect_memeq "t4_omiss" t4_omiss $
        fmap (t4_map) t4_imiss,
        expect_memeq "t4_oerr" t4_oerr $
        fmap (t4_map) t4_ierr],
      test "Finally" [
        expect_memeq "t5_ohit" t5_ohit $
        (t5_ihit `BNF.finally` t5_map) t5_in,
        expect_memeq "t5_omiss" t5_omiss $
        (t5_imiss `BNF.finally` t5_map) t5_in,
        expect_memeq "t5_oerr" t5_oerr $
        (t5_ierr `BNF.finally` t5_map) t5_in],
      test "Rep" [
        expect_memeq "t6_outA" t6_outA $
        (BNF.rep t6_reps t6_hit) t6_inA,
        expect_memeq "t6_outB" t6_outB $
        (BNF.rep t6_reps t6_hit) t6_inB,
        expect_memeq "t6_outC" t6_outC $
        (BNF.rep t6_reps t6_hit) t6_inC],
      test "ZeroOne" [
        expect_memeq "t7_outA" t7_outA $
        (BNF.zero_one t7_hit) t7_inA,
        expect_memeq "t7_outB" t7_outB $
        (BNF.zero_one t7_hit) t7_inB],
      test "ZeroMore" [
        expect_memeq "t8_outA" t8_outA $
        (BNF.zero_more t8_hit) t8_inA,
        expect_memeq "t8_outB" t8_outB $
        (BNF.zero_more t8_hit) t8_inB,
        expect_memeq "t8_outC" t8_outC $
        (BNF.zero_more t8_hit) t8_inC],
      test "OneMore" [
        expect_memeq "t9_outA" t9_outA $
        (BNF.one_more t9_hit) t9_inA,
        expect_memeq "t9_outB" t9_outB $
        (BNF.one_more t9_hit) t9_inB,
        expect_memeq "t9_outC" t9_outC $
        (BNF.one_more t9_hit) t9_inC]]
