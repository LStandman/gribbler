-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023 LStandman

module JSON() where

import qualified BNF as BNF
import BNF.Text

data JSON =
  JObject JSON     |
  JArray  JSON     |
  JSring  String   |
  JNumber Rational |
  JTrue            |
  JFalse           |
  JNull

json =
  element

value =
  object             `BNF.or`
  array              `BNF.or`
  string             `BNF.or`
  number             `BNF.or`
  match_text "true"  `BNF.or`
  match_text "false" `BNF.or`
  match_text "null"

object :: BNF.Parser String DiffString
object =
   match_char '{' `BNF.and` ws      `BNF.and` match_char '}' `BNF.or`
  (match_char '{' `BNF.and` members `BNF.and` match_char '}')

members :: BNF.Parser String DiffString
members =
  member `BNF.or`
  member `BNF.and` match_char ',' `BNF.and` members

member :: BNF.Parser String DiffString
member =
  ws `BNF.and` string `BNF.and` ws `BNF.and` match_char ':' `BNF.and` element

array :: BNF.Parser String DiffString
array =
   match_char '[' `BNF.and` ws       `BNF.and` match_char ']' `BNF.or`
  (match_char '[' `BNF.and` elements `BNF.and` match_char ']')

elements :: BNF.Parser String DiffString
elements =
  element `BNF.or`
  (element `BNF.and` match_char ',' `BNF.and` elements)

element :: BNF.Parser String DiffString
element =
  ws `BNF.and` value `BNF.and` ws

string :: BNF.Parser String DiffString
string =
  match_char '"' `BNF.and` characters `BNF.and` match_char '"'

characters :: BNF.Parser String DiffString
characters =
  BNF.zom (
    character)

character :: BNF.Parser String DiffString
character =
  any_char ['\x0020'..'\x10FFFF'] `BNF.except` match_char '"' `BNF.except` match_char '\\' `BNF.or`
  (match_char '\\' `BNF.and` escape)

escape :: BNF.Parser String DiffString
escape =
  match_char '"'  `BNF.or`
  match_char '\\' `BNF.or`
  match_char '/'  `BNF.or`
  match_char 'b'  `BNF.or`
  match_char 'f'  `BNF.or`
  match_char 'n'  `BNF.or`
  match_char 'r'  `BNF.or`
  match_char 't'  `BNF.or`
  (match_char 'u' `BNF.and` BNF.rep 4 hex)

hex :: BNF.Parser String DiffString
hex =
  digit `BNF.or`
  any_char ['A'..'F'] `BNF.or`
  any_char ['a'..'f']

number :: BNF.Parser String DiffString
number =
  integer `BNF.and` fraction `BNF.and` JSON.exponent

integer :: BNF.Parser String DiffString
integer =
  digit `BNF.or`
  (onenine `BNF.and` digits) `BNF.or`
  (match_char '-' `BNF.and` digit) `BNF.or`
  (match_char '-' `BNF.and` onenine `BNF.and` digits)

digits :: BNF.Parser String DiffString
digits =
  BNF.oom (
    digit)

digit :: BNF.Parser String DiffString
digit =
  match_char '0' `BNF.or`
  onenine

onenine :: BNF.Parser String DiffString
onenine =
  any_char ['1'..'9']

fraction :: BNF.Parser String DiffString
fraction =
  BNF.zoo (
    match_char '.' `BNF.and` digits)

exponent :: BNF.Parser String DiffString
exponent =
  BNF.zoo (
     match_char 'E' `BNF.and` sign `BNF.and` digits `BNF.or`
    (match_char 'e' `BNF.and` sign `BNF.and` digits))

sign :: BNF.Parser String DiffString
sign =
  BNF.zoo (
    match_char '+' `BNF.or`
    match_char '-')

ws :: BNF.Parser String DiffString
ws =
  BNF.zom (
    match_char '\x0020' `BNF.or`
    match_char '\x000A' `BNF.or`
    match_char '\x000D' `BNF.or`
    match_char '\x0009')
