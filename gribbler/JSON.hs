-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023 LStandman

module JSON(
    BNF.Result(..),
    JSValue(..),
    json)
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

json s = BNF.eval_parser element s

value :: BNF.Parser String JSValue
value =
  BNF.throw "Could not deduce JSON type" (
    object                               `BNF.or`
    array                                `BNF.or`
    (string >>= return . JSString)       `BNF.or`
    number                               `BNF.or`
    (get_text "true"  >> return JSTrue)  `BNF.or`
    (get_text "false" >> return JSFalse) `BNF.or`
    (get_text "null"  >> return JSNull))

object :: BNF.Parser String JSValue
object =
  meta_char '{' `BNF.and` (members `BNF.or` ws) `BNF.and`
    (BNF.throw "Unterminated braces '{}'" $ meta_char '}') >>=
      return . JSObject . relist

members :: BNF.Parser String (DiffList (String, JSValue))
members =
  (member >>= return . difflist . (:[])) `BNF.and`
    BNF.zoo (meta_char ',' `BNF.and` members)

member :: BNF.Parser String (String, JSValue)
member =
  ws `BNF.and` string `BNF.and` ws `BNF.and` meta_char ':' >>=
    \ s -> element >>= \ e -> return (s, e)

array :: BNF.Parser String JSValue
array =
  meta_char '[' `BNF.and` (elements `BNF.or` ws) `BNF.and`
    (BNF.throw "Unterminated brackets '[]'" $ meta_char ']') >>=
      return . JArray . relist

elements :: BNF.Parser String (DiffList JSValue)
elements =
  (element >>= return . difflist. (:[])) `BNF.and`
    BNF.zoo (meta_char ',' `BNF.and` elements)

element :: BNF.Parser String JSValue
element =
  (ws :: TextParser) >>
    value >>= \ v ->
      (ws :: TextParser) >>
        return v

string :: BNF.Parser String String
string =
  meta_char '"' `BNF.and` characters `BNF.and`
    (BNF.throw "Unterminated string" $ meta_char '"') >>=
      return . relist

characters :: TextParser
characters = BNF.zom (character)

character :: TextParser
character =
  (BNF.throw "Unsupported character" $ get_char_in_range ('\x0020', '\x10FFFF'))
    `BNF.excl` get_char '"' `BNF.excl` get_char '\\' `BNF.or`
  (meta_char '\\' `BNF.and` escape)

escape :: TextParser
escape =
  BNF.throw "Unsupported escape sequence" $
    ((meta_char '"'  :: TextParser) >> (return $ difflist ['"']))  `BNF.or`
    ((meta_char '\\' :: TextParser) >> (return $ difflist ['\\'])) `BNF.or`
    ((meta_char 'b'  :: TextParser) >> (return $ difflist ['\b'])) `BNF.or`
    ((meta_char 'f'  :: TextParser) >> (return $ difflist ['\f'])) `BNF.or`
    ((meta_char 'n'  :: TextParser) >> (return $ difflist ['\n'])) `BNF.or`
    ((meta_char 'r'  :: TextParser) >> (return $ difflist ['\r'])) `BNF.or`
    ((meta_char 't'  :: TextParser) >> (return $ difflist ['\t'])) `BNF.or`
    (meta_char 'u' `BNF.and` BNF.rep 4 hex >>=
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
digits = BNF.oom (digit)

digit :: TextParser
digit = get_char '0' `BNF.or` onenine

onenine :: TextParser
onenine = get_any_char ['1'..'9']

fraction :: TextParser
fraction = BNF.zoo (get_char '.' `BNF.and` digits)

exponent :: TextParser
exponent =
  BNF.zoo (
     get_char 'E' `BNF.and` sign `BNF.and` digits `BNF.or`
    (get_char 'e' `BNF.and` sign `BNF.and` digits))

sign :: TextParser
sign = BNF.zoo (get_char '+' `BNF.or` get_char '-')

ws :: Monoid a => BNF.Parser String a
ws =
  BNF.zom (
    meta_char '\x0020' `BNF.or`
    meta_char '\x000A' `BNF.or`
    meta_char '\x000D' `BNF.or`
    meta_char '\x0009')
