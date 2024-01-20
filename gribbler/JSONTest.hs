-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSONTest.hs: Unit tests for JSON module.
-- Copyright (C) 2023-2024 LStandman

module JSONTest(test_json) where

import Data.Either

import GHC.Stack
import JSON
import qualified JSON.BNF as BNF
import JSON.BNFTest
import Libtest

test_json =
  runtests [
    test_bnf,
    test_json']

test_json' =
  let
    t01_string = "{\
\  \"first_name\": \"John\",\
\  \"last_name\": \"Smith\",\
\  \"is_alive\": true,\
\  \"age\": 27,\
\  \"address\": {\
\    \"street_address\": \"21 2nd Street\",\
\    \"city\": \"New York\",\
\    \"state\": \"NY\",\
\    \"postal_code\": \"10021-3100\"\
\  },\
\  \"phone_numbers\": [\
\    {\
\      \"type\": \"home\",\
\      \"number\": \"212 555-1234\"\
\    },\
\    {\
\      \"type\": \"office\",\
\      \"number\": \"646 555-4567\"\
\    }\
\  ],\
\  \"children\": [\
\    \"Catherine\",\
\    \"Thomas\",\
\    \"Trevor\"\
\  ],\
\  \"spouse\": null\
\}"
    t01_json = JSObject [
      ("first_name", JSString "John"),
      ("last_name",  JSString "Smith"),
      ("is_alive",   JSTrue),
      ("age",        JSNumber "27"),
      ( "address",
        JSObject [
          ("street_address", JSString "21 2nd Street"),
          ("city",           JSString "New York"),
          ("state",          JSString "NY"),
          ("postal_code",    JSString "10021-3100")]),
          ( "phone_numbers",
            JSArray [
              JSObject [
                ("type",   JSString "home"),
                ("number", JSString "212 555-1234")
              ],
              JSObject [
                ("type",   JSString "office"),
                ("number", JSString "646 555-4567")
              ]
          ]),
      ( "children",
        JSArray [
          JSString "Catherine",
          JSString "Thomas",
          JSString "Trevor"
        ]),
      ("spouse", JSNull)]
    t02_string = "[\
\    \"JSON Test Pattern pass1\",\
\    {\"object with 1 member\":[\"array with 1 element\"]},\
\    {},\
\    [],\
\    -42,\
\    true,\
\    false,\
\    null,\
\    {\
\        \"integer\": 1234567890,\
\        \"real\": -9876.543210,\
\        \"e\": 0.123456789e-12,\
\        \"E\": 1.234567890E+34,\
\        \"\":  23456789012E66,\
\        \"zero\": 0,\
\        \"one\": 1,\
\        \"space\": \" \",\
\        \"quote\": \"\\\"\",\
\        \"backslash\": \"\\\\\",\
\        \"controls\": \"\\b\\f\\n\\r\\t\",\
\        \"slash\": \"/\",\
\        \"alpha\": \"abcdefghijklmnopqrstuvwyz\",\
\        \"ALPHA\": \"ABCDEFGHIJKLMNOPQRSTUVWYZ\",\
\        \"digit\": \"0123456789\",\
\        \"0123456789\": \"digit\",\
\        \"special\": \"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",\
\        \"hex\": \"\\u0123\\u4567\\u89AB\\uCDEF\\uabcd\\uef4A\",\
\        \"true\": true,\
\        \"false\": false,\
\        \"null\": null,\
\        \"array\":[  ],\
\        \"object\":{  },\
\        \"address\": \"50 St. James Street\",\
\        \"url\": \"http://www.JSON.org/\",\
\        \"comment\": \"// /* <!-- --\",\
\        \"# -- --> */\": \" \",\
\        \" s p a c e d \" :[1,2 , 3\
\\
\,\
\\
\4 , 5        ,          6           ,7        ],\"compact\":[1,2,3,4,5,6,7],\
\        \"jsontext\": \"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",\
\        \"quotes\": \"&#34; \\u0022 %22 0x22 034 &#x22;\",\
\        \"/\\\\\\\"\\uCAFE\\uBABE\\uAB98\\uFCDE\\ubcda\\uef4A\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\"\
\: \"A key can be any string\"\
\    },\
\    0.5 ,98.6\
\,\
\99.44\
\,\
\\
\1066,\
\1e1,\
\0.1e1,\
\1e-1,\
\1e00,2e+00,2e-00\
\,\"rosebud\"]"
    t02_json = JSArray [
      JSString "JSON Test Pattern pass1",
      JSObject [
        ( "object with 1 member",
          JSArray [
            JSString "array with 1 element"
          ])
      ],
      JSObject [],
      JSArray [],
      JSNumber "-42",
      JSTrue,
      JSFalse,
      JSNull,
      JSObject [
        ("integer",     JSNumber "1234567890"),
        ("real",        JSNumber "-9876.543210"),
        ("e",           JSNumber "0.123456789e-12"),
        ("E",           JSNumber "1.234567890E+34"),
        ("",            JSNumber "23456789012E66"),
        ("zero",        JSNumber "0"),
        ("one",         JSNumber "1"),
        ("space",       JSString " "),
        ("quote",       JSString "\""),
        ("backslash",   JSString "\\"),
        ("controls",    JSString "\b\f\n\r\t"),
        ("slash",       JSString "/"),
        ("alpha",       JSString "abcdefghijklmnopqrstuvwyz"),
        ("ALPHA",       JSString "ABCDEFGHIJKLMNOPQRSTUVWYZ"),
        ("digit",       JSString "0123456789"),
        ("0123456789",  JSString "digit"),
        ("special",     JSString "`1~!@#$%^&*()_+-={':[,]}|;.</>?"),
        ("hex",         JSString "\x0123\x4567\x89AB\xCDEF\xABCD\xEF4A"),
        ("true",        JSTrue),
        ("false",       JSFalse),
        ("null",        JSNull),
        ("array",       JSArray []),
        ("object",      JSObject []),
        ("address",     JSString "50 St. James Street"),
        ("url",         JSString "http://www.JSON.org/"),
        ("comment",     JSString "// /* <!-- --"),
        ("# -- --> */", JSString " "),
        ( " s p a c e d ",
          JSArray [
            JSNumber "1",
            JSNumber "2",
            JSNumber "3",
            JSNumber "4",
            JSNumber "5",
            JSNumber "6",
            JSNumber "7"
          ]),
        ( "compact",
          JSArray [
            JSNumber "1",
            JSNumber "2",
            JSNumber "3",
            JSNumber "4",
            JSNumber "5",
            JSNumber "6",
            JSNumber "7"
          ]),
        ( "jsontext",
          JSString "{\"object with 1 member\":[\"array with 1 element\"]}"),
        ( "quotes",
          JSString "&#34; \x0022 %22 0x22 034 &#x22;"),
        ( "/\\\"\xCAFE\xBABE\xAB98\xFCDE\xBCDA\xEF4A\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?",
          JSString "A key can be any string")
      ],
      JSNumber "0.5",
      JSNumber "98.6",
      JSNumber "99.44",
      JSNumber "1066",
      JSNumber "1e1",
      JSNumber "0.1e1",
      JSNumber "1e-1",
      JSNumber "1e00",
      JSNumber "2e+00",
      JSNumber "2e-00",
      JSString "rosebud"]
    t03_string = "[\"Unclosed array\""
    t04_string = "{unquoted_key: \"keys must be quoted\"}"
    t05_string = "[\"extra comma\",]"
    t06_string = "[\"double extra comma\",,]"
    t07_string = "[   , \"<-- missing value\"]"
    t08_string = "[\"Comma after the close\"],"
    t09_string = "[\"Extra close\"]]"
    t10_string = "{\"Extra comma\": true,}"
    t11_string = "{\"Extra value after close\": true} \"misplaced quoted value\""
    t12_string = "{\"Illegal expression\": 1 + 2}"
    t13_string = "{\"Illegal invocation\": alert()}"
    t14_string = "{\"Numbers cannot have leading zeroes\": 013}"
    t15_string = "{\"Numbers cannot be hex\": 0x14}"
    t16_string = "[\"Illegal backslash escape: \\x15\"]"
    t17_string = "[\\naked]"
    t18_string = "[\"Illegal backslash escape: \\017\"]"
    t19_string = "{\"Missing colon\" null}"
    t20_string = "{\"Double colon\":: null}"
    t21_string = "{\"Comma instead of colon\", null}"
    t22_string = "[\"Colon instead of comma\": false]"
    t23_string = "[\"Bad value\", truth]"
    t24_string = "['single quote']"
    t25_string = "[\"\ttab\tcharacter\tin\tstring\t\"]"
    t26_string = "[\"tab\\\tcharacter\\\tin\\\tstring\\\t\"]"
    t27_string = "[\"line\nbreak\"]"
    t28_string = "[\"line\\\nbreak\"]"
    t29_string = "[0e]"
    t30_string = "[0e+]"
    t31_string = "[0e+-1]"
    t32_string = "{\"Comma instead if closing brace\": true,"
    t33_string = "[\"mismatch\"}"
    t34_json   = t02_json
    t35_json   = t02_json
  in
    testsuite "JSON" [
      test "DeserializationWikipedia" [
        expect_memeq "t01_json" (Right t01_json) $ deserialize t01_string],
      test "DeserializationGaloisIncPass" [
        expect_memeq "t02_json" (Right t02_json) $ deserialize t02_string],
      test "DeserializationGaloisIncFail" [
        expect_false $ isRight $ deserialize t03_string,
        expect_false $ isRight $ deserialize t04_string,
        expect_false $ isRight $ deserialize t05_string,
        expect_false $ isRight $ deserialize t06_string,
        expect_false $ isRight $ deserialize t07_string,
        expect_false $ isRight $ deserialize t08_string,
        expect_false $ isRight $ deserialize t09_string,
        expect_false $ isRight $ deserialize t10_string,
        expect_false $ isRight $ deserialize t11_string,
        expect_false $ isRight $ deserialize t12_string,
        expect_false $ isRight $ deserialize t13_string,
        expect_false $ isRight $ deserialize t14_string,
        expect_false $ isRight $ deserialize t15_string,
        expect_false $ isRight $ deserialize t16_string,
        expect_false $ isRight $ deserialize t17_string,
        expect_false $ isRight $ deserialize t18_string,
        expect_false $ isRight $ deserialize t19_string,
        expect_false $ isRight $ deserialize t20_string,
        expect_false $ isRight $ deserialize t21_string,
        expect_false $ isRight $ deserialize t22_string,
        expect_false $ isRight $ deserialize t23_string,
        expect_false $ isRight $ deserialize t24_string,
        expect_false $ isRight $ deserialize t25_string,
        expect_false $ isRight $ deserialize t26_string,
        expect_false $ isRight $ deserialize t27_string,
        expect_false $ isRight $ deserialize t28_string,
        expect_false $ isRight $ deserialize t29_string,
        expect_false $ isRight $ deserialize t30_string,
        expect_false $ isRight $ deserialize t31_string,
        expect_false $ isRight $ deserialize t32_string,
        expect_false $ isRight $ deserialize t33_string],
      test "SerializationGaloisInc" [
        expect_memeq "t34_json" (Right t34_json) $ deserialize $ serialize True  t34_json,
        expect_memeq "t35_json" (Right t35_json) $ deserialize $ serialize False t35_json]]
