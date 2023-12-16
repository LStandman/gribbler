-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023 LStandman

module JSON(
    JValue(..),
    json,
    string,
    characters)
  where

import Data.Maybe

import qualified BNF as BNF
import BNF.Text
import DiffList
import MemUtils

newtype SafeString =
    SafeString String
  deriving (Eq, Show)

instance Semigroup SafeString
  where
    (SafeString a) <> (SafeString b) = SafeString (a <> b)

instance Monoid SafeString
  where
    mempty = SafeString mempty

newtype SafeDict a =
    SafeDict [(SafeString, a)]
  deriving (Eq, Show)

data JValue =
    JObject (SafeDict JValue) |
    JArray  [JValue]          |
    JString SafeString        |
    JNumber String            |
    JTrue                     |
    JFalse                    |
    JNull
  deriving (Eq, Show)

json :: String -> BNF.Result JValue

json s =
  BNF.eval_parser element s

value :: BNF.Parser String JValue
value =
  object                                      `BNF.or`
  array                                       `BNF.or`
  (string >>= \ s -> return $ JString s)      `BNF.or`
  number                                      `BNF.or`
  (get_text "true"  >>= \ _ -> return JTrue)  `BNF.or`
  (get_text "false" >>= \ _ -> return JFalse) `BNF.or`
  (get_text "null"  >>= \ _ -> return JNull)

object :: BNF.Parser String JValue
object =
   drop_char '{' `BNF.and` ws      `BNF.and` drop_char '}' `BNF.or`
  (drop_char '{' `BNF.and` members `BNF.and` drop_char '}') >>=
  \ ms -> return . JObject . SafeDict $ relist ms

members :: BNF.Parser String (DiffList (SafeString, JValue))
members =
  (member >>= \ m -> return $ difflist [m]) `BNF.and`
  BNF.zoo (drop_char ',' `BNF.and` members)

member :: BNF.Parser String (SafeString, JValue)
member =
  ws `BNF.and` string `BNF.and` ws `BNF.and` drop_char ':' >>=
  \ s -> element >>= \ e -> return (s, e)

array :: BNF.Parser String JValue
array =
   drop_char '[' `BNF.and` ws       `BNF.and` drop_char ']' `BNF.or`
  (drop_char '[' `BNF.and` elements `BNF.and` drop_char ']') >>=
  \ es -> return . JArray $ relist es

elements :: BNF.Parser String (DiffList JValue)
elements =
  (element >>= \ e -> return $ difflist [e]) `BNF.and`
  BNF.zoo (drop_char ',' `BNF.and` elements)

element :: BNF.Parser String JValue
element =
  (ws :: TextParser) >>= \ _ ->
    value >>= \ v ->
      (ws :: TextParser) >>= \ _ ->
        return v

string :: BNF.Parser String SafeString
string =
  drop_char '"' `BNF.and` characters `BNF.and` drop_char '"' >>=
  \ s -> return . SafeString . relist $ s

characters :: TextParser
characters =
  BNF.zom (
    character)

character :: TextParser
character =
  get_any_char ['\x0020'..'\x10FFFF'] `BNF.except` get_char '"' `BNF.except` get_char '\\' `BNF.or`
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
    \ s -> return . difflist $ [toEnum . fromJust . hex2num . relist $ s])
  

hex :: TextParser
hex =
  digit `BNF.or`
  get_any_char ['A'..'F'] `BNF.or`
  get_any_char ['a'..'f']

number :: BNF.Parser String JValue
number =
  integer `BNF.and` fraction `BNF.and` JSON.exponent >>=
  \ s -> return . JNumber . relist $ s

integer :: TextParser
integer =
  digit `BNF.or`
  (onenine `BNF.and` digits) `BNF.or`
  (get_char '-' `BNF.and` digit) `BNF.or`
  (get_char '-' `BNF.and` onenine `BNF.and` digits)

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

ws :: Monoid a => BNF.Parser String a
ws =
  BNF.drop (BNF.zom (
    get_char '\x0020' `BNF.or`
    get_char '\x000A' `BNF.or`
    get_char '\x000D' `BNF.or`
    get_char '\x0009'))
