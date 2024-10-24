-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNFTest.hs: Unit tests for BNF
-- Copyright (C) 2022-2023 LStandman

module JSON.BNFTest (testBnf) where

import Data.Char
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

testBnf =
  let errorX = BNF.Error "X" :: TextResult
      errorY = BNF.Error "Y" :: TextResult
      miss0 = BNF.Miss :: TextResult
      emit_errX = BNF.Parser (return errorX)
      emit_errY = BNF.Parser (return errorY)
      emit_miss0 = BNF.Parser (return miss0)
      getA = JSON.BNFTest.getChar 'a'
      getB = JSON.BNFTest.getChar 'b'
      t1_in = "a"
      t1_resultA = BNF.Hit (difflist "a", "")
      t1_resultB = BNF.Hit (difflist "b", "")
      t1_onAreturnB = JSON.BNFTest.getChar 'a' >> return (difflist "b")
      t1_hitXhit = t1_resultA
      t1_hitXmiss = t1_resultA
      t1_hitXerr = t1_resultA
      t1_missXhit = t1_resultB
      t1_missXmiss = miss0
      t1_missXerr = errorY
      t1_errXhit = errorX
      t1_errXmiss = errorX
      t1_errXerr = errorX
      t2_in = "ab"
      t2_success = BNF.Hit (difflist "ab", "")
      t2_hitXhit = t2_success
      t2_hitXmiss = miss0
      t2_hitXerr = errorY
      t2_missXhit = miss0
      t2_missXmiss = miss0
      t2_missXerr = miss0
      t2_errXhit = errorX
      t2_errXmiss = errorX
      t2_errXerr = errorX
      t3_in = "a"
      t3_success = BNF.Hit (difflist "a", "")
      t3_hitXhit = miss0
      t3_hitXmiss = t3_success
      t3_hitXerr = errorY
      t3_missXhit = miss0
      t3_missXmiss = miss0
      t3_missXerr = miss0
      t3_errXhit = errorX
      t3_errXmiss = errorX
      t3_errXerr = errorX
      t4_reps = 2
      t4_in1 = "abb"
      t4_out1 = BNF.Miss
      t4_in2 = "aab"
      t4_out2 = BNF.Hit (difflist "aa", "b")
      t4_in3 = "aaa"
      t4_out3 = BNF.Hit (difflist "aa", "a")
      t5_hit = JSON.BNFTest.getChar 'a'
      t5_in1 = "aa"
      t5_out1 = BNF.Hit (difflist "a", "a")
      t5_in2 = "ba"
      t5_out2 = BNF.Hit (difflist "", "ba")
      t6_hit = JSON.BNFTest.getChar 'a'
      t6_in1 = "bbb"
      t6_out1 = BNF.Hit (difflist "", "bbb")
      t6_in2 = "abb"
      t6_out2 = BNF.Hit (difflist "a", "bb")
      t6_in3 = "aab"
      t6_out3 = BNF.Hit (difflist "aa", "b")
      t7_in1 = "bbb"
      t7_out1 = BNF.Miss
      t7_in2 = "abb"
      t7_out2 = BNF.Hit (difflist "a", "bb")
      t7_in3 = "aab"
      t7_out3 = BNF.Hit (difflist "aa", "b")
      t8_in = "a"
      t8_e = "Z"
      t8_hit = BNF.Hit (difflist "a", "")
      t8_miss = BNF.Error t8_e :: BNF.Result (DiffString, String)
      t8_error = errorX
   in testsuite
        "BNF"
        [ test
            "Or"
            [ expectVarEq "t1_hitXhit" t1_hitXhit $
                BNF.runParser (getA `BNF.or` t1_onAreturnB) t1_in,
              expectVarEq "t1_hitXmiss" t1_hitXmiss $
                BNF.runParser (getA `BNF.or` emit_miss0) t1_in,
              expectVarEq "t1_hitXerr" t1_hitXerr $
                BNF.runParser (getA `BNF.or` emit_errY) t1_in,
              expectVarEq "t1_missXhit" t1_missXhit $
                BNF.runParser (emit_miss0 `BNF.or` t1_onAreturnB) t1_in,
              expectVarEq "t1_missXmiss" t1_missXmiss $
                BNF.runParser (emit_miss0 `BNF.or` emit_miss0) t1_in,
              expectVarEq "t1_missXerr" t1_missXerr $
                BNF.runParser (emit_miss0 `BNF.or` emit_errY) t1_in,
              expectVarEq "t1_errXhit" t1_errXhit $
                BNF.runParser (emit_errX `BNF.or` t1_onAreturnB) t1_in,
              expectVarEq "t1_errXmiss" t1_errXmiss $
                BNF.runParser (emit_errX `BNF.or` emit_miss0) t1_in,
              expectVarEq "t1_errXerr" t1_errXerr $
                BNF.runParser (emit_errX `BNF.or` emit_errY) t1_in
            ],
          test
            "And"
            [ expectVarEq "t2_hitXhit" t2_hitXhit $
                BNF.runParser (getA `BNF.and` getB) t2_in,
              expectVarEq "t2_hitXmiss" t2_hitXmiss $
                BNF.runParser (getA `BNF.and` emit_miss0) t2_in,
              expectVarEq "t2_hitXerr" t2_hitXerr $
                BNF.runParser (getA `BNF.and` emit_errY) t2_in,
              expectVarEq "t2_missXhit" t2_missXhit $
                BNF.runParser (emit_miss0 `BNF.and` getB) t2_in,
              expectVarEq "t2_missXmiss" t2_missXmiss $
                BNF.runParser (emit_miss0 `BNF.and` emit_miss0) t2_in,
              expectVarEq "t2_missXerr" t2_missXerr $
                BNF.runParser (emit_miss0 `BNF.and` emit_errY) t2_in,
              expectVarEq "t2_errXhit" t2_errXhit $
                BNF.runParser (emit_errX `BNF.and` getB) t2_in,
              expectVarEq "t2_errXmiss" t2_errXmiss $
                BNF.runParser (emit_errX `BNF.and` emit_miss0) t2_in,
              expectVarEq "t2_errXerr" t2_errXerr $
                BNF.runParser (emit_errX `BNF.and` emit_errY) t2_in
            ],
          test
            "Excl"
            [ expectVarEq "t3_hitXhit" t3_hitXhit $
                BNF.runParser (getA `BNF.excl` getA) t3_in,
              expectVarEq "t3_hitXmiss" t3_hitXmiss $
                BNF.runParser (getA `BNF.excl` emit_miss0) t3_in,
              expectVarEq "t3_hitXerr" t3_hitXerr $
                BNF.runParser (getA `BNF.excl` emit_errY) t3_in,
              expectVarEq "t3_missXhit" t3_missXhit $
                BNF.runParser (emit_miss0 `BNF.excl` getA) t3_in,
              expectVarEq "t3_missXmiss" t3_missXmiss $
                BNF.runParser (emit_miss0 `BNF.excl` emit_miss0) t3_in,
              expectVarEq "t3_missXerr" t3_missXerr $
                BNF.runParser (emit_miss0 `BNF.excl` emit_errY) t3_in,
              expectVarEq "t3_errXhit" t3_errXhit $
                BNF.runParser (emit_errX `BNF.excl` getA) t3_in,
              expectVarEq "t3_errXmiss" t3_errXmiss $
                BNF.runParser (emit_errX `BNF.excl` emit_miss0) t3_in,
              expectVarEq "t3_errXerr" t3_errXerr $
                BNF.runParser (emit_errX `BNF.excl` emit_errY) t3_in
            ],
          test
            "Rep"
            [ expectVarEq "t4_out1" t4_out1 $
                BNF.runParser (BNF.rep t4_reps getA) t4_in1,
              expectVarEq "t4_out2" t4_out2 $
                BNF.runParser (BNF.rep t4_reps getA) t4_in2,
              expectVarEq "t4_out3" t4_out3 $
                BNF.runParser (BNF.rep t4_reps getA) t4_in3
            ],
          test
            "ZOO"
            [ expectVarEq "t5_out1" t5_out1 $
                BNF.runParser (BNF.zoo t5_hit) t5_in1,
              expectVarEq "t5_out2" t5_out2 $
                BNF.runParser (BNF.zoo t5_hit) t5_in2
            ],
          test
            "ZOM"
            [ expectVarEq "t6_out1" t6_out1 $
                BNF.runParser (BNF.zom t6_hit) t6_in1,
              expectVarEq "t6_out2" t6_out2 $
                BNF.runParser (BNF.zom t6_hit) t6_in2,
              expectVarEq "t6_out3" t6_out3 $
                BNF.runParser (BNF.zom t6_hit) t6_in3
            ],
          test
            "OOM"
            [ expectVarEq "t7_out1" t7_out1 $
                BNF.runParser (BNF.oom getA) t7_in1,
              expectVarEq "t7_out2" t7_out2 $
                BNF.runParser (BNF.oom getA) t7_in2,
              expectVarEq "t7_out3" t7_out3 $
                BNF.runParser (BNF.oom getA) t7_in3
            ],
          test
            "Assert"
            [ expectVarEq "t8_hit" t8_hit $
                BNF.runParser (BNF.assert t8_e getA) t8_in,
              expectVarEq "t8_miss" t8_miss $
                BNF.runParser (BNF.assert t8_e emit_miss0) t8_in,
              expectVarEq "t8_error" t8_error $
                BNF.runParser (BNF.assert t8_e emit_errX) t8_in
            ]
        ]
