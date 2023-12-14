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
    errorX        = BNF.Error "X" :: TextResult
    errorY        = BNF.Error "Y" :: TextResult
    amiss         = BNF.Miss :: TextResult
    errX          = return errorX
    errY          = return errorY
    miss          = return amiss
    t1_in         = ""
    t1_successA   = BNF.Hit (difflist "a", "")
    t1_successB   = BNF.Hit (difflist "b", "")
    t1_hitA       = return t1_successA
    t1_hitB       = return t1_successB
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
    t2_hitA       = match_char 'a'
    t2_hitB       = match_char 'b'
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
    t3_hitA       = match_char 'a'
    t3_hitB       = match_char 'a'
    t3_hitxhit    = amiss
    t3_hitxmiss   = t3_success
    t3_hitxerr    = errorY
    t3_missxhit   = amiss
    t3_missxmiss  = amiss
    t3_missxerr   = amiss
    t3_errxhit    = errorX
    t3_errxmiss   = errorX
    t3_errxerr    = errorX
    t4_map        = length . relist
    t4_ihit       = BNF.Hit (difflist "abc", "xyz")
    t4_ohit       = BNF.Hit (3, "xyz")
    t4_imiss      = BNF.Miss :: TextResult
    t4_omiss      = BNF.Miss :: BNF.Result String Int
    t4_ierr       = BNF.Error "X" :: TextResult
    t4_oerr       = BNF.Error "X" :: BNF.Result String Int
    t5_in         = ""
    t5_map        = t4_map
    t5_ihit       = return t4_ihit
    t5_ohit       = t4_ohit
    t5_imiss      = return t4_imiss
    t5_omiss      = t4_omiss
    t5_ierr       = return t4_ierr
    t5_oerr       = t4_oerr
    t6_hit        = match_char 'a'
    t6_reps       = 2
    t6_inA        = "abb"
    t6_outA       = BNF.Miss
    t6_inB        = "aab"
    t6_outB       = BNF.Hit (difflist "aa", "b")
    t6_inC        = "aaa"
    t6_outC       = BNF.Hit (difflist "aa", "a")
    t7_hit        = match_char 'a'
    t7_inA        = "aa"
    t7_outA       = BNF.Hit (difflist "a", "a")
    t7_inB        = "ba"
    t7_outB       = BNF.Hit (difflist "", "ba")
    t8_hit        = match_char 'a'
    t8_inA        = "bbb"
    t8_outA       = BNF.Hit (difflist "", "bbb")
    t8_inB        = "abb"
    t8_outB       = BNF.Hit (difflist "a", "bb")
    t8_inC        = "aab"
    t8_outC       = BNF.Hit (difflist "aa", "b")
    t9_hit        = match_char 'a'
    t9_inA        = "bbb"
    t9_outA       = BNF.Miss
    t9_inB        = "abb"
    t9_outB       = BNF.Hit (difflist "a", "bb")
    t9_inC        = "aab"
    t9_outC       = BNF.Hit (difflist "aa", "b")
    t10_in        = ""
    t10_errStr    = "Y"
    t10_errA      = BNF.Error "X"
    t10_errB      = BNF.Error t10_errStr
    t10_fhit      = return $ BNF.Hit (difflist "a", "")
    t10_ohit      = t10_errB
    t10_fmiss     = return BNF.Miss :: TextParser
    t10_omiss     = BNF.Miss
    t10_ferr      = return t10_errA :: TextParser
    t10_oerr      = t10_errA
    t11_in        = "a"
    t11_f         = (\ (x, s) -> (length $ relist x, s))
    t11_success   = BNF.Hit (1, "")
    t11_hitA      = match_char 'a'
    t11_hit       = t11_success
    t11_miss      = BNF.Miss :: BNF.Result String Int
    t11_err       = BNF.Error "X" :: BNF.Result String Int
    t12_in        = "a"
    t12_f         = BNF.Hit . (\ (x, s) -> (length $ relist x, s))
    t12_success   = BNF.Hit (1, "")
    t12_hitA      = match_char 'a'
    t12_hit       = t12_success
    t12_miss      = BNF.Miss :: BNF.Result String Int
    t12_err       = BNF.Error "X" :: BNF.Result String Int
  in
    testsuite "BNF" [
      test "Or" [
        expect_memeq "t1_hitxhit" t1_hitxhit $
        (t1_hitA `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_hitxmiss" t1_hitxmiss $
        (t1_hitA `BNF.or` miss) t1_in,
        expect_memeq "t1_hitxerr" t1_hitxerr $
        (t1_hitA `BNF.or` errY) t1_in,
        expect_memeq "t1_missxhit" t1_missxhit $
        (miss `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_missxmiss" t1_missxmiss $
        (miss `BNF.or` miss) t1_in,
        expect_memeq "t1_missxerr" t1_missxerr $
        (miss `BNF.or` errY) t1_in,
        expect_memeq "t1_errxhit" t1_errxhit $
        (errX `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_errxmiss" t1_errxmiss $
        (errX `BNF.or` miss) t1_in,
        expect_memeq "t1_errxerr" t1_errxerr $
        (errX `BNF.or` errY) t1_in],
      test "And" [
        expect_memeq "t2_hitxhit" t2_hitxhit $
        (t2_hitA `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_hitxmiss" t2_hitxmiss $
        (t2_hitA `BNF.and` miss) t2_in,
        expect_memeq "t2_hitxerr" t2_hitxerr $
        (t2_hitA `BNF.and` errY) t2_in,
        expect_memeq "t2_missxhit" t2_missxhit $
        (miss `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_missxmiss" t2_missxmiss $
        (miss `BNF.and` miss) t2_in,
        expect_memeq "t2_missxerr" t2_missxerr $
        (miss `BNF.and` errY) t2_in,
        expect_memeq "t2_errxhit" t2_errxhit $
        (errX `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_errxmiss" t2_errxmiss $
        (errX `BNF.and` miss) t2_in,
        expect_memeq "t2_errxerr" t2_errxerr $
        (errX `BNF.and` errY) t2_in],
      test "Except" [
        expect_memeq "t3_hitxhit" t3_hitxhit $
        (t3_hitA `BNF.except` t3_hitB) t3_in,
        expect_memeq "t3_hitxmiss" t3_hitxmiss $
        (t3_hitA `BNF.except` miss) t3_in,
        expect_memeq "t3_hitxerr" t3_hitxerr $
        (t3_hitA `BNF.except` errY) t3_in,
        expect_memeq "t3_missxhit" t3_missxhit $
        (miss `BNF.except` t3_hitB) t3_in,
        expect_memeq "t3_missxmiss" t3_missxmiss $
        (miss `BNF.except` miss) t3_in,
        expect_memeq "t3_missxerr" t3_missxerr $
        (miss `BNF.except` errY) t3_in,
        expect_memeq "t3_errxhit" t3_errxhit $
        (errX `BNF.except` t3_hitB) t3_in,
        expect_memeq "t3_errxmiss" t3_errxmiss $
        (errX `BNF.except` miss) t3_in,
        expect_memeq "t3_errxerr" t3_errxerr $
        (errX `BNF.except` errY) t3_in],
      test "Fmap" [
        expect_memeq "t4_ohit" t4_ohit $
        fmap (t4_map) t4_ihit,
        expect_memeq "t4_omiss" t4_omiss $
        fmap (t4_map) t4_imiss,
        expect_memeq "t4_oerr" t4_oerr $
        fmap (t4_map) t4_ierr],
      test "Conv" [
        expect_memeq "t5_ohit" t5_ohit $
        (t5_ihit `BNF.conv` t5_map) t5_in,
        expect_memeq "t5_omiss" t5_omiss $
        (t5_imiss `BNF.conv` t5_map) t5_in,
        expect_memeq "t5_oerr" t5_oerr $
        (t5_ierr `BNF.conv` t5_map) t5_in],
      test "Rep" [
        expect_memeq "t6_outA" t6_outA $
        (BNF.rep t6_reps t6_hit) t6_inA,
        expect_memeq "t6_outB" t6_outB $
        (BNF.rep t6_reps t6_hit) t6_inB,
        expect_memeq "t6_outC" t6_outC $
        (BNF.rep t6_reps t6_hit) t6_inC],
      test "ZOO" [
        expect_memeq "t7_outA" t7_outA $
        (BNF.zoo t7_hit) t7_inA,
        expect_memeq "t7_outB" t7_outB $
        (BNF.zoo t7_hit) t7_inB],
      test "ZOM" [
        expect_memeq "t8_outA" t8_outA $
        (BNF.zom t8_hit) t8_inA,
        expect_memeq "t8_outB" t8_outB $
        (BNF.zom t8_hit) t8_inB,
        expect_memeq "t8_outC" t8_outC $
        (BNF.zom t8_hit) t8_inC],
      test "OOM" [
        expect_memeq "t9_outA" t9_outA $
        (BNF.oom t9_hit) t9_inA,
        expect_memeq "t9_outB" t9_outB $
        (BNF.oom t9_hit) t9_inB,
        expect_memeq "t9_outC" t9_outC $
        (BNF.oom t9_hit) t9_inC],
      test "Err" [
        expect_memeq "t10_ohit" t10_ohit $
        (t10_fhit `BNF.err` t10_errStr) t10_in,
        expect_memeq "t10_omiss" t10_omiss $
        (t10_fmiss `BNF.err` t10_errStr) t10_in,
        expect_memeq "t10_oerr" t10_oerr $
        (t10_ferr `BNF.err` t10_errStr) t10_in],
      test "Finally" [
        expect_memeq "t11_hit" t11_hit $
        (t11_hitA `BNF.finally` t11_f) t11_in,
        expect_memeq "t11_miss" t11_miss $
        (miss `BNF.finally` t11_f) t11_in,
        expect_memeq "t11_err" t11_err $
        (errX `BNF.finally` t11_f) t11_in],
      test "on_hit" [
        expect_memeq "t12_hit" t12_hit $
        (t12_hitA `BNF.on_hit` t12_f) t12_in,
        expect_memeq "t12_miss" t12_miss $
        (miss `BNF.on_hit` t12_f) t12_in,
        expect_memeq "t12_err" t12_err $
        (errX `BNF.on_hit` t12_f) t12_in]]
