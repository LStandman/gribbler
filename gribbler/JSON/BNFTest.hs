-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNFTest.hs: Unit tests for BNF
-- Copyright (C) 2022-2023 LStandman

module JSON.BNFTest (test_bnf) where

import Data.Char
--
import qualified JSON.BNF as BNF
import Libtest
import Misc.DiffList

type TextParser = BNF.Parser String DiffString

type TextResult = BNF.Result (DiffString, String)

getChar :: Char -> BNF.Parser String DiffString
getChar c =
  BNF.Parser $
    \xs ->
      case xs of
        [] -> BNF.Miss
        (y : ys) -> (if c == y then return (difflist [c], ys) else BNF.Miss)

test_bnf =
  let errorX = BNF.Error "X" :: TextResult
      errorY = BNF.Error "Y" :: TextResult
      miss0 = BNF.Miss :: TextResult
      emit_errX = BNF.Parser (return errorX)
      emit_errY = BNF.Parser (return errorY)
      emit_miss0 = BNF.Parser (return miss0)
      t1_in = "a"
      t1_successA = BNF.Hit (difflist "a", "")
      t1_successB = BNF.Hit (difflist "b", "")
      t1_hitA = JSON.BNFTest.getChar 'a'
      t1_hitB = JSON.BNFTest.getChar 'a' >> return (difflist "b")
      t1_hitxhit = t1_successA
      t1_hitxmiss = t1_successA
      t1_hitxerr = t1_successA
      t1_missxhit = t1_successB
      t1_missxmiss = miss0
      t1_missxerr = errorY
      t1_errxhit = errorX
      t1_errxmiss = errorX
      t1_errxerr = errorX
      t2_in = "ab"
      t2_success = BNF.Hit (difflist "ab", "")
      t2_hitA = JSON.BNFTest.getChar 'a'
      t2_hitB = JSON.BNFTest.getChar 'b'
      t2_hitxhit = t2_success
      t2_hitxmiss = miss0
      t2_hitxerr = errorY
      t2_missxhit = miss0
      t2_missxmiss = miss0
      t2_missxerr = miss0
      t2_errxhit = errorX
      t2_errxmiss = errorX
      t2_errxerr = errorX
      t3_in = "a"
      t3_success = BNF.Hit (difflist "a", "")
      t3_hitA = JSON.BNFTest.getChar 'a'
      t3_hitB = JSON.BNFTest.getChar 'a'
      t3_hitxhit = miss0
      t3_hitxmiss = t3_success
      t3_hitxerr = errorY
      t3_missxhit = miss0
      t3_missxmiss = miss0
      t3_missxerr = miss0
      t3_errxhit = errorX
      t3_errxmiss = errorX
      t3_errxerr = errorX
      t4_hit = JSON.BNFTest.getChar 'a'
      t4_reps = 2
      t4_inA = "abb"
      t4_outA = BNF.Miss
      t4_inB = "aab"
      t4_outB = BNF.Hit (difflist "aa", "b")
      t4_inC = "aaa"
      t4_outC = BNF.Hit (difflist "aa", "a")
      t5_hit = JSON.BNFTest.getChar 'a'
      t5_inA = "aa"
      t5_outA = BNF.Hit (difflist "a", "a")
      t5_inB = "ba"
      t5_outB = BNF.Hit (difflist "", "ba")
      t6_hit = JSON.BNFTest.getChar 'a'
      t6_inA = "bbb"
      t6_outA = BNF.Hit (difflist "", "bbb")
      t6_inB = "abb"
      t6_outB = BNF.Hit (difflist "a", "bb")
      t6_inC = "aab"
      t6_outC = BNF.Hit (difflist "aa", "b")
      t7_hit = JSON.BNFTest.getChar 'a'
      t7_inA = "bbb"
      t7_outA = BNF.Miss
      t7_inB = "abb"
      t7_outB = BNF.Hit (difflist "a", "bb")
      t7_inC = "aab"
      t7_outC = BNF.Hit (difflist "aa", "b")
      t8_in = "a"
      t8_e = "Z"
      t8_hitA = JSON.BNFTest.getChar 'a'
      t8_hit = BNF.Hit (difflist "a", "")
      t8_miss = BNF.Error t8_e :: BNF.Result (DiffString, String)
      t8_error = errorX
   in testsuite
        "BNF"
        [ test
            "Or"
            [ expectVarEq "t1_hitxhit" t1_hitxhit $
                BNF.runParser (t1_hitA `BNF.or` t1_hitB) t1_in,
              expectVarEq "t1_hitxmiss" t1_hitxmiss $
                BNF.runParser (t1_hitA `BNF.or` emit_miss0) t1_in,
              expectVarEq "t1_hitxerr" t1_hitxerr $
                BNF.runParser (t1_hitA `BNF.or` emit_errY) t1_in,
              expectVarEq "t1_missxhit" t1_missxhit $
                BNF.runParser (emit_miss0 `BNF.or` t1_hitB) t1_in,
              expectVarEq "t1_missxmiss" t1_missxmiss $
                BNF.runParser (emit_miss0 `BNF.or` emit_miss0) t1_in,
              expectVarEq "t1_missxerr" t1_missxerr $
                BNF.runParser (emit_miss0 `BNF.or` emit_errY) t1_in,
              expectVarEq "t1_errxhit" t1_errxhit $
                BNF.runParser (emit_errX `BNF.or` t1_hitB) t1_in,
              expectVarEq "t1_errxmiss" t1_errxmiss $
                BNF.runParser (emit_errX `BNF.or` emit_miss0) t1_in,
              expectVarEq "t1_errxerr" t1_errxerr $
                BNF.runParser (emit_errX `BNF.or` emit_errY) t1_in
            ],
          test
            "And"
            [ expectVarEq "t2_hitxhit" t2_hitxhit $
                BNF.runParser (t2_hitA `BNF.and` t2_hitB) t2_in,
              expectVarEq "t2_hitxmiss" t2_hitxmiss $
                BNF.runParser (t2_hitA `BNF.and` emit_miss0) t2_in,
              expectVarEq "t2_hitxerr" t2_hitxerr $
                BNF.runParser (t2_hitA `BNF.and` emit_errY) t2_in,
              expectVarEq "t2_missxhit" t2_missxhit $
                BNF.runParser (emit_miss0 `BNF.and` t2_hitB) t2_in,
              expectVarEq "t2_missxmiss" t2_missxmiss $
                BNF.runParser (emit_miss0 `BNF.and` emit_miss0) t2_in,
              expectVarEq "t2_missxerr" t2_missxerr $
                BNF.runParser (emit_miss0 `BNF.and` emit_errY) t2_in,
              expectVarEq "t2_errxhit" t2_errxhit $
                BNF.runParser (emit_errX `BNF.and` t2_hitB) t2_in,
              expectVarEq "t2_errxmiss" t2_errxmiss $
                BNF.runParser (emit_errX `BNF.and` emit_miss0) t2_in,
              expectVarEq "t2_errxerr" t2_errxerr $
                BNF.runParser (emit_errX `BNF.and` emit_errY) t2_in
            ],
          test
            "Excl"
            [ expectVarEq "t3_hitxhit" t3_hitxhit $
                BNF.runParser (t3_hitA `BNF.excl` t3_hitB) t3_in,
              expectVarEq "t3_hitxmiss" t3_hitxmiss $
                BNF.runParser (t3_hitA `BNF.excl` emit_miss0) t3_in,
              expectVarEq "t3_hitxerr" t3_hitxerr $
                BNF.runParser (t3_hitA `BNF.excl` emit_errY) t3_in,
              expectVarEq "t3_missxhit" t3_missxhit $
                BNF.runParser (emit_miss0 `BNF.excl` t3_hitB) t3_in,
              expectVarEq "t3_missxmiss" t3_missxmiss $
                BNF.runParser (emit_miss0 `BNF.excl` emit_miss0) t3_in,
              expectVarEq "t3_missxerr" t3_missxerr $
                BNF.runParser (emit_miss0 `BNF.excl` emit_errY) t3_in,
              expectVarEq "t3_errxhit" t3_errxhit $
                BNF.runParser (emit_errX `BNF.excl` t3_hitB) t3_in,
              expectVarEq "t3_errxmiss" t3_errxmiss $
                BNF.runParser (emit_errX `BNF.excl` emit_miss0) t3_in,
              expectVarEq "t3_errxerr" t3_errxerr $
                BNF.runParser (emit_errX `BNF.excl` emit_errY) t3_in
            ],
          test
            "Rep"
            [ expectVarEq "t4_outA" t4_outA $
                BNF.runParser (BNF.rep t4_reps t4_hit) t4_inA,
              expectVarEq "t4_outB" t4_outB $
                BNF.runParser (BNF.rep t4_reps t4_hit) t4_inB,
              expectVarEq "t4_outC" t4_outC $
                BNF.runParser (BNF.rep t4_reps t4_hit) t4_inC
            ],
          test
            "ZOO"
            [ expectVarEq "t5_outA" t5_outA $
                BNF.runParser (BNF.zoo t5_hit) t5_inA,
              expectVarEq "t5_outB" t5_outB $
                BNF.runParser (BNF.zoo t5_hit) t5_inB
            ],
          test
            "ZOM"
            [ expectVarEq "t6_outA" t6_outA $
                BNF.runParser (BNF.zom t6_hit) t6_inA,
              expectVarEq "t6_outB" t6_outB $
                BNF.runParser (BNF.zom t6_hit) t6_inB,
              expectVarEq "t6_outC" t6_outC $
                BNF.runParser (BNF.zom t6_hit) t6_inC
            ],
          test
            "OOM"
            [ expectVarEq "t7_outA" t7_outA $
                BNF.runParser (BNF.oom t7_hit) t7_inA,
              expectVarEq "t7_outB" t7_outB $
                BNF.runParser (BNF.oom t7_hit) t7_inB,
              expectVarEq "t7_outC" t7_outC $
                BNF.runParser (BNF.oom t7_hit) t7_inC
            ],
          test
            "Assert"
            [ expectVarEq "t8_hit" t8_hit $
                BNF.runParser (BNF.assert t8_e t8_hitA) t8_in,
              expectVarEq "t8_miss" t8_miss $
                BNF.runParser (BNF.assert t8_e emit_miss0) t8_in,
              expectVarEq "t8_error" t8_error $
                BNF.runParser (BNF.assert t8_e emit_errX) t8_in
            ]
        ]
