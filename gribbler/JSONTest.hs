-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSONTest.hs: Unit tests for JSON module.
-- Copyright (C) 2023 LStandman

module JSONTest(test_json) where

import JSON
import qualified JSON.BNF as BNF
import JSON.BNFTest
import Libtest

test_json' :: IO Bool

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
    t1_json = BNF.Hit (JSObject [
      ("first_name", JSString "John"),
      ("last_name", JSString "Smith"),
      ("is_alive", JSTrue),
      ("age", JSNumber "27"),
      ("address",JSObject [
        ("street_address", JSString "21 2nd Street"),
        ("city", JSString "New York"),
        ("state", JSString "NY"),
        ("postal_code", JSString "10021-3100")]),
        ("phone_numbers", JArray [
          JSObject [
            ("type", JSString "home"),
            ("number", JSString "212 555-1234")
          ],
          JSObject [
            ("type", JSString "office"),
            ("number", JSString "646 555-4567")
          ]
        ]),
      ("children", JArray [
        JSString "Catherine",
        JSString "Thomas",
        JSString "Trevor"]),
      ("spouse", JSNull)])
  in
    testsuite "JSON" [
      test "Wikipedia" [
        expect_memeq "t1_json" t1_json $ json t1_string]]
