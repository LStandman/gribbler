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

to_jsstring1 :: HasCallStack => String -> [JSChar]
to_jsstring1 s = case to_jsstring s of
  Right js -> js
  Left  e  -> error e

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
    t1_json = BNF.Hit (JSObject [
      (to_jsstring1 "first_name", JSString $ to_jsstring1 "John"),
      (to_jsstring1 "last_name", JSString $ to_jsstring1 "Smith"),
      (to_jsstring1 "is_alive", JSTrue),
      (to_jsstring1 "age", JSNumber "27"),
      (to_jsstring1 "address",JSObject [
        (to_jsstring1 "street_address", JSString $ to_jsstring1 "21 2nd Street"),
        (to_jsstring1 "city", JSString $ to_jsstring1 "New York"),
        (to_jsstring1 "state", JSString $ to_jsstring1 "NY"),
        (to_jsstring1 "postal_code", JSString $ to_jsstring1 "10021-3100")]),
        (to_jsstring1 "phone_numbers", JArray [
          JSObject [
            (to_jsstring1 "type", JSString $ to_jsstring1 "home"),
            (to_jsstring1 "number", JSString $ to_jsstring1 "212 555-1234")
          ],
          JSObject [
            (to_jsstring1 "type", JSString $ to_jsstring1 "office"),
            (to_jsstring1 "number", JSString $ to_jsstring1 "646 555-4567")
          ]
        ]),
      (to_jsstring1 "children", JArray [
        JSString $ to_jsstring1 "Catherine",
        JSString $ to_jsstring1 "Thomas",
        JSString $ to_jsstring1 "Trevor"]),
      (to_jsstring1 "spouse", JSNull)])
  in
    testsuite "JSON" [
      test "Wikipedia" [
        expect_memeq "t1_json" t1_json $ json t1_string]]
