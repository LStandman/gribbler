-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/DicewareTest.hs: Unit tests for Diceware
-- Copyright (C) 2021-2023 LStandman

module Misc.DicewareTest (test_diceware) where

import Data.Either
--
import Libtest
import qualified Misc.Diceware as Diceware

test_diceware =
  let t1_dictionary =
        [ "blue",
          "cyan",
          "green",
          "orange",
          "purple",
          "red",
          "white",
          "yellow"
        ]
      t1_number = 0o0577
      t1_hits = ["blue", "red", "yellow", "yellow"]
      t1_misses = ["blue", "red", "cat", "yellow"]
      t1_unsorted =
        [ "blue",
          "cyan",
          "orange",
          "green",
          "purple",
          "red",
          "white",
          "yellow"
        ]
      t1_repeating =
        [ "blue",
          "cyan",
          "green",
          "green",
          "purple",
          "red",
          "white",
          "yellow"
        ]
      t1_uppercase =
        [ "blue",
          "cyan",
          "green",
          "orAnge",
          "purple",
          "red",
          "white",
          "yellow"
        ]
      t1_numeric =
        [ "blue",
          "cyan",
          "green",
          "or1nge",
          "purple",
          "red",
          "white",
          "yellow"
        ]
      t1_space =
        [ "blue",
          "cyan",
          "green",
          "or nge",
          "purple",
          "red",
          "white",
          "yellow"
        ]
      t1_symbol =
        [ "blue",
          "cyan",
          "green",
          "or@nge",
          "purple",
          "red",
          "white",
          "yellow"
        ]
   in testsuite
        "Diceware"
        [ test
            "DecodePass"
            [ expectVarEq "t1_number" (Just t1_number) $
                Diceware.decode t1_dictionary t1_hits
            ],
          test
            "DecodeFailNotInDictionary"
            [ expectVarEq "t1_number" Nothing $
                Diceware.decode t1_dictionary t1_misses
            ],
          test
            "EncodePass"
            [ expectVarEq "t1_hits" t1_hits $
                Diceware.encode t1_dictionary (length t1_hits) t1_number
            ],
          test
            "IsSanitizedPass"
            [ expectTrue $ isRight $ Diceware.isSanitized t1_dictionary
            ],
          test
            "IsSanitizedFailUnsorted"
            [ expectFalse $ isRight $ Diceware.isSanitized t1_unsorted
            ],
          test
            "IsSanitizedFailRepeating"
            [ expectFalse $ isRight $ Diceware.isSanitized t1_repeating
            ],
          test
            "IsSanitizedFailUppercase"
            [ expectFalse $ isRight $ Diceware.isSanitized t1_uppercase
            ],
          test
            "IsSanitizedFailNumeric"
            [ expectFalse $ isRight $ Diceware.isSanitized t1_numeric
            ],
          test
            "IsSanitizedFailSpace"
            [ expectFalse $ isRight $ Diceware.isSanitized t1_space
            ],
          test
            "IsSanitizedFailSymbol"
            [ expectFalse $ isRight $ Diceware.isSanitized t1_symbol
            ]
        ]
