-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNFTest.hs: Unit tests for BNF
-- Copyright (C) 2022-2023 LStandman

module JSON.BNFTest(test_bnf) where

import Data.Char
--
import qualified JSON.BNF as BNF
import Libtest
import Misc.DiffList

type TextParser = BNF.Parser String DiffString
type TextResult = BNF.Result (DiffString, String)

get_char :: Char -> BNF.Parser String DiffString
get_char c = BNF.Parser (
  \ xs -> case xs of
    []     -> BNF.Miss
    (y:ys) -> case c == y of
      True  -> return (difflist [c], ys)
      False -> BNF.Miss)

test_bnf =
  let
    errorX       = BNF.Error "X" :: TextResult
    errorY       = BNF.Error "Y" :: TextResult
    miss0        = BNF.Miss :: TextResult
    emit_errX    = BNF.Parser (return errorX)
    emit_errY    = BNF.Parser (return errorY)
    emit_miss0   = BNF.Parser (return miss0)
    t1_in        = "a"
    t1_successA  = BNF.Hit (difflist "a", "")
    t1_successB  = BNF.Hit (difflist "b", "")
    t1_hitA      = get_char 'a'
    t1_hitB      = get_char 'a' >> (return $ difflist "b")
    t1_hitxhit   = t1_successA
    t1_hitxmiss  = t1_successA
    t1_hitxerr   = t1_successA
    t1_missxhit  = t1_successB
    t1_missxmiss = miss0
    t1_missxerr  = errorY
    t1_errxhit   = errorX
    t1_errxmiss  = errorX
    t1_errxerr   = errorX
    t2_in        = "ab"
    t2_success   = BNF.Hit (difflist "ab", "")
    t2_hitA      = get_char 'a'
    t2_hitB      = get_char 'b'
    t2_hitxhit   = t2_success
    t2_hitxmiss  = miss0
    t2_hitxerr   = errorY
    t2_missxhit  = miss0
    t2_missxmiss = miss0
    t2_missxerr  = miss0
    t2_errxhit   = errorX
    t2_errxmiss  = errorX
    t2_errxerr   = errorX
    t3_in        = "a"
    t3_success   = BNF.Hit (difflist "a", "")
    t3_hitA      = get_char 'a'
    t3_hitB      = get_char 'a'
    t3_hitxhit   = miss0
    t3_hitxmiss  = t3_success
    t3_hitxerr   = errorY
    t3_missxhit  = miss0
    t3_missxmiss = miss0
    t3_missxerr  = miss0
    t3_errxhit   = errorX
    t3_errxmiss  = errorX
    t3_errxerr   = errorX
    t4_hit       = get_char 'a'
    t4_reps      = 2
    t4_inA       = "abb"
    t4_outA      = BNF.Miss
    t4_inB       = "aab"
    t4_outB      = BNF.Hit (difflist "aa", "b")
    t4_inC       = "aaa"
    t4_outC      = BNF.Hit (difflist "aa", "a")
    t5_hit       = get_char 'a'
    t5_inA       = "aa"
    t5_outA      = BNF.Hit (difflist "a", "a")
    t5_inB       = "ba"
    t5_outB      = BNF.Hit (difflist "", "ba")
    t6_hit       = get_char 'a'
    t6_inA       = "bbb"
    t6_outA      = BNF.Hit (difflist "", "bbb")
    t6_inB       = "abb"
    t6_outB      = BNF.Hit (difflist "a", "bb")
    t6_inC       = "aab"
    t6_outC      = BNF.Hit (difflist "aa", "b")
    t7_hit       = get_char 'a'
    t7_inA       = "bbb"
    t7_outA      = BNF.Miss
    t7_inB       = "abb"
    t7_outB      = BNF.Hit (difflist "a", "bb")
    t7_inC       = "aab"
    t7_outC      = BNF.Hit (difflist "aa", "b")
    t8_in        = "a"
    t8_e         = "Z"
    t8_hitA      = get_char 'a'
    t8_hit       = BNF.Hit (difflist "a", "")
    t8_miss      = BNF.Error t8_e :: BNF.Result (DiffString, String)
    t8_error     = errorX
  in
    testsuite "BNF" [
      test "Or" [
        expect_memeq "t1_hitxhit" t1_hitxhit $
        BNF.run_parser (t1_hitA `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_hitxmiss" t1_hitxmiss $
        BNF.run_parser (t1_hitA `BNF.or` emit_miss0) t1_in,
        expect_memeq "t1_hitxerr" t1_hitxerr $
        BNF.run_parser (t1_hitA `BNF.or` emit_errY) t1_in,
        expect_memeq "t1_missxhit" t1_missxhit $
        BNF.run_parser (emit_miss0 `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_missxmiss" t1_missxmiss $
        BNF.run_parser (emit_miss0 `BNF.or` emit_miss0) t1_in,
        expect_memeq "t1_missxerr" t1_missxerr $
        BNF.run_parser (emit_miss0 `BNF.or` emit_errY) t1_in,
        expect_memeq "t1_errxhit" t1_errxhit $
        BNF.run_parser (emit_errX `BNF.or` t1_hitB) t1_in,
        expect_memeq "t1_errxmiss" t1_errxmiss $
        BNF.run_parser (emit_errX `BNF.or` emit_miss0) t1_in,
        expect_memeq "t1_errxerr" t1_errxerr $
        BNF.run_parser (emit_errX `BNF.or` emit_errY) t1_in],
      test "And" [
        expect_memeq "t2_hitxhit" t2_hitxhit $
        BNF.run_parser (t2_hitA `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_hitxmiss" t2_hitxmiss $
        BNF.run_parser (t2_hitA `BNF.and` emit_miss0) t2_in,
        expect_memeq "t2_hitxerr" t2_hitxerr $
        BNF.run_parser (t2_hitA `BNF.and` emit_errY) t2_in,
        expect_memeq "t2_missxhit" t2_missxhit $
        BNF.run_parser (emit_miss0 `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_missxmiss" t2_missxmiss $
        BNF.run_parser (emit_miss0 `BNF.and` emit_miss0) t2_in,
        expect_memeq "t2_missxerr" t2_missxerr $
        BNF.run_parser (emit_miss0 `BNF.and` emit_errY) t2_in,
        expect_memeq "t2_errxhit" t2_errxhit $
        BNF.run_parser (emit_errX `BNF.and` t2_hitB) t2_in,
        expect_memeq "t2_errxmiss" t2_errxmiss $
        BNF.run_parser (emit_errX `BNF.and` emit_miss0) t2_in,
        expect_memeq "t2_errxerr" t2_errxerr $
        BNF.run_parser (emit_errX `BNF.and` emit_errY) t2_in],
      test "Excl" [
        expect_memeq "t3_hitxhit" t3_hitxhit $
        BNF.run_parser (t3_hitA `BNF.excl` t3_hitB) t3_in,
        expect_memeq "t3_hitxmiss" t3_hitxmiss $
        BNF.run_parser (t3_hitA `BNF.excl` emit_miss0) t3_in,
        expect_memeq "t3_hitxerr" t3_hitxerr $
        BNF.run_parser (t3_hitA `BNF.excl` emit_errY) t3_in,
        expect_memeq "t3_missxhit" t3_missxhit $
        BNF.run_parser (emit_miss0 `BNF.excl` t3_hitB) t3_in,
        expect_memeq "t3_missxmiss" t3_missxmiss $
        BNF.run_parser (emit_miss0 `BNF.excl` emit_miss0) t3_in,
        expect_memeq "t3_missxerr" t3_missxerr $
        BNF.run_parser (emit_miss0 `BNF.excl` emit_errY) t3_in,
        expect_memeq "t3_errxhit" t3_errxhit $
        BNF.run_parser (emit_errX `BNF.excl` t3_hitB) t3_in,
        expect_memeq "t3_errxmiss" t3_errxmiss $
        BNF.run_parser (emit_errX `BNF.excl` emit_miss0) t3_in,
        expect_memeq "t3_errxerr" t3_errxerr $
        BNF.run_parser (emit_errX `BNF.excl` emit_errY) t3_in],
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
      test "Try" [
        expect_memeq "t8_hit" t8_hit $
        BNF.run_parser (BNF.try t8_e t8_hitA) t8_in,
        expect_memeq "t8_miss" t8_miss $
        BNF.run_parser (BNF.try t8_e emit_miss0) t8_in,
        expect_memeq "t8_error" t8_error $
        BNF.run_parser (BNF.try t8_e emit_errX) t8_in]]
