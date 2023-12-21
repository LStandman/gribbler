-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023 LStandman

module JSON(
    BNF.from_hit,
    JSValue(..),
    json,
    string,
    characters)
  where

import Data.Maybe

import qualified JSON.BNF as BNF
import JSON.BNF.Text
import Misc.DiffList
import Misc.MemUtils

data JSValue =
    JSObject [(String, JSValue)] |
    JArray  [JSValue]            |
    JSString String              |
    JSNumber String              |
    JSTrue                       |
    JSFalse                      |
    JSNull
  deriving (Eq, Show)

json :: String -> BNF.Result JSValue

json s =
  BNF.eval_parser (
  (ws :: TextParser) >>= \ _ ->
    value >>= \ v ->
      (ws_or_eof :: TextParser) >>= \ _ ->
        return v) s

value :: BNF.Parser String JSValue
value =
  object                                       `BNF.or`
  array                                        `BNF.or`
  (string >>= return . JSString)               `BNF.or`
  number                                       `BNF.or`
  (get_text "true"  >>= \ _ -> return JSTrue)  `BNF.or`
  (get_text "false" >>= \ _ -> return JSFalse) `BNF.or`
  (get_text "null"  >>= \ _ -> return JSNull)

object :: BNF.Parser String JSValue
object =
   drop_char '{' `BNF.and` ws      `BNF.and` drop_char '}' `BNF.or`
  (drop_char '{' `BNF.and` members `BNF.and` drop_char '}') >>=
    return . JSObject . relist

members :: BNF.Parser String (DiffList (String, JSValue))
members =
  (member >>= return . difflist . (:[])) `BNF.and`
  BNF.zoo (drop_char ',' `BNF.and` members)

member :: BNF.Parser String (String, JSValue)
member =
  ws `BNF.and` string `BNF.and` ws `BNF.and` drop_char ':' >>=
    \ s -> element >>= \ e -> return (s, e)

array :: BNF.Parser String JSValue
array =
   drop_char '[' `BNF.and` ws       `BNF.and` drop_char ']' `BNF.or`
  (drop_char '[' `BNF.and` elements `BNF.and` drop_char ']') >>=
    return . JArray . relist

elements :: BNF.Parser String (DiffList JSValue)
elements =
  (element >>= return . difflist. (:[])) `BNF.and`
  BNF.zoo (drop_char ',' `BNF.and` elements)

element :: BNF.Parser String JSValue
element =
  (ws :: TextParser) >>= \ _ ->
    value >>= \ v ->
      (ws :: TextParser) >>= \ _ ->
        return v

string :: BNF.Parser String String
string =
  drop_char '"' `BNF.and` characters `BNF.and` drop_char '"' >>=
    return . relist

characters :: TextParser
characters =
  BNF.zom (
    character)

character :: TextParser
character =
  get_char_in_range ('\x0020', '\x10FFFF') `BNF.except` get_char '"' `BNF.except` get_char '\\' `BNF.or`
  (drop_char '\\' `BNF.and` escape)

escape :: TextParser
escape =
  (get_char '"'  >>= \ _ -> return $ difflist ['"'])  `BNF.or`
  (get_char '\\' >>= \ _ -> return $ difflist ['\\']) `BNF.or`
  (get_char 'b'  >>= \ _ -> return $ difflist ['\b']) `BNF.or`
  (get_char 'f'  >>= \ _ -> return $ difflist ['\f']) `BNF.or`
  (get_char 'n'  >>= \ _ -> return $ difflist ['\n']) `BNF.or`
  (get_char 'r'  >>= \ _ -> return $ difflist ['\r']) `BNF.or`
  (get_char 't'  >>= \ _ -> return $ difflist ['\t']) `BNF.or`
  ( drop_char 'u' `BNF.and` BNF.rep 4 hex >>=
      return . difflist . (:[]) . toEnum . fromJust . hex2num . relist)

hex :: TextParser
hex =
  digit `BNF.or`
  get_any_char ['A'..'F'] `BNF.or`
  get_any_char ['a'..'f']

number :: BNF.Parser String JSValue
number =
  integer `BNF.and` fraction `BNF.and` JSON.exponent >>=
    return . JSNumber . relist

integer :: TextParser
integer =
  (onenine `BNF.and` digits) `BNF.or`
  digit `BNF.or`
  (get_char '-' `BNF.and` onenine `BNF.and` digits) `BNF.or`
  (get_char '-' `BNF.and` digit)

digits :: TextParser
digits =
  BNF.oom (
    digit)

digit :: TextParser
digit =
  get_char '0' `BNF.or`
  onenine

onenine :: TextParser
onenine =
  get_any_char ['1'..'9']

fraction :: TextParser
fraction =
  BNF.zoo (
    get_char '.' `BNF.and` digits)

exponent :: TextParser
exponent =
  BNF.zoo (
     get_char 'E' `BNF.and` sign `BNF.and` digits `BNF.or`
    (get_char 'e' `BNF.and` sign `BNF.and` digits))

sign :: TextParser
sign =
  BNF.zoo (
    get_char '+' `BNF.or`
    get_char '-')

ws' :: Monoid a => BNF.Parser String a
ws' =
  BNF.drop (
    get_char '\x0020' `BNF.or`
    get_char '\x000A' `BNF.or`
    get_char '\x000D' `BNF.or`
    get_char '\x0009')

ws :: Monoid a => BNF.Parser String a
ws =
  BNF.zom ws'

ws_or_eof :: Monoid a => BNF.Parser String a
ws_or_eof =
  get_eof `BNF.or` (ws' `BNF.and` ws_or_eof)
