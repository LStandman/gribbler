-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSONTest.hs: Unit tests for JSON module.
-- Copyright (C) 2023 LStandman

module JSONTest(test_json) where

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
    t1_string = "{\
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
    t1_json = Right (JSObject [
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
            JArray [
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
        JArray [
          JSString "Catherine",
          JSString "Thomas",
          JSString "Trevor"
        ]),
      ("spouse", JSNull)])
    t2_string = "[\
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
    t2_json = Right (JArray [
      JSString "JSON Test Pattern pass1",
      JSObject [
        ( "object with 1 member",
          JArray [
            JSString "array with 1 element"
          ])
      ],
      JSObject [],
      JArray [],
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
        ("array",       JArray []),
        ("object",      JSObject []),
        ("address",     JSString "50 St. James Street"),
        ("url",         JSString "http://www.JSON.org/"),
        ("comment",     JSString "// /* <!-- --"),
        ("# -- --> */", JSString " "),
        ( " s p a c e d ",
          JArray [
            JSNumber "1",
            JSNumber "2",
            JSNumber "3",
            JSNumber "4",
            JSNumber "5",
            JSNumber "6",
            JSNumber "7"
          ]),
        ( "compact",
          JArray [
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
      JSString "rosebud"])
  in
    testsuite "JSON" [
      test "Wikipedia" [
        expect_memeq "t1_json" t1_json $ json t1_string],
      test "Galois Inc." [
        expect_memeq "t2_json" t2_json $ json t2_string]
      ]
