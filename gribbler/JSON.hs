-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023 LStandman

module JSON(
    JSValue(..),
    json)
  where

import Data.Char
import Data.Maybe

import qualified JSON.BNF as BNF
import JSON.BNF.Text
import Misc.DiffList
import Misc.MemUtils

data JSValue =
    JSObject [(String, JSValue)] |
    JArray [JSValue]             |
    JSString String              |
    JSNumber String              |
    JSTrue                       |
    JSFalse                      |
    JSNull
  deriving (Eq, Show)

json :: String -> Either String JSValue

json s =
  case BNF.eval_parser element $ text_state s of
    BNF.Hit   j -> Right j
    BNF.Error e -> Left e
    BNF.Miss    -> Left "Unspecified error"

value :: BNF.Parser TextState JSValue
value =
  object                               `BNF.or`
  array                                `BNF.or`
  (string >>= return . JSString)       `BNF.or`
  (get_text "true"  >> return JSTrue)  `BNF.or`
  (get_text "false" >> return JSFalse) `BNF.or`
  (get_text "null"  >> return JSNull)  `BNF.or`
  number

object :: BNF.Parser TextState JSValue
object =
  assert_push (meta_char '{') `BNF.and` (members `BNF.or` ws) `BNF.and`
    assert_pop "Unterminated braces '{}'" (meta_char '}') >>=
      return . JSObject . relist

members :: BNF.Parser TextState (DiffList (String, JSValue))
members =
  (member >>= return . difflist . (:[])) `BNF.and`
    BNF.zoo (meta_char ',' `BNF.and` members)

member :: BNF.Parser TextState (String, JSValue)
member =
  ws `BNF.and` string `BNF.and` ws `BNF.and` meta_char ':' >>=
    \ s -> element >>= \ e -> return (s, e)

array :: BNF.Parser TextState JSValue
array =
  assert_push (meta_char '[') `BNF.and` (elements `BNF.or` ws) `BNF.and`
    assert_pop "Unterminated brackets '[]'" (meta_char ']') >>=
      return . JArray . relist

elements :: BNF.Parser TextState (DiffList JSValue)
elements =
  (element >>= return . difflist. (:[])) `BNF.and`
    BNF.zoo (meta_char ',' `BNF.and` elements)

element :: BNF.Parser TextState JSValue
element =
  (ws :: BNF.Parser TextState ()) >>
    value >>= \ v ->
      (ws :: BNF.Parser TextState ()) >>
        return v

string :: BNF.Parser TextState String
string =
  assert_push (meta_char '"') `BNF.and` characters `BNF.and`
    assert_pop "Unterminated string" (meta_char '"') >>=
      return . relist

characters :: BNF.Parser TextState DiffString
characters = BNF.zom (character >>= return . difflist. (:[]))

character :: BNF.Parser TextState Char
character =
  (meta_char '\\' :: BNF.Parser TextState ()) >> escape `BNF.or`
  (assert_noop "Unsupported character" (get_char_with (\ c -> ('\x0020' <= c) && (c <= '\x10FFFF')))
    `BNF.excl` (meta_char '"' :: BNF.Parser TextState ())) >>=
      return

escape :: BNF.Parser TextState Char
escape =
  assert_noop "Unsupported escape sequence" $
    ((meta_char '"'  :: BNF.Parser TextState ()) >> (return '"'))  `BNF.or`
    ((meta_char '\\' :: BNF.Parser TextState ()) >> (return '\\')) `BNF.or`
    ((meta_char 'b'  :: BNF.Parser TextState ()) >> (return '\b')) `BNF.or`
    ((meta_char 'f'  :: BNF.Parser TextState ()) >> (return '\f')) `BNF.or`
    ((meta_char 'n'  :: BNF.Parser TextState ()) >> (return '\n')) `BNF.or`
    ((meta_char 'r'  :: BNF.Parser TextState ()) >> (return '\r')) `BNF.or`
    ((meta_char 't'  :: BNF.Parser TextState ()) >> (return '\t')) `BNF.or`
    ((meta_char 'u'  :: BNF.Parser TextState ()) >> BNF.rep 4 hex >>=
      return . toEnum . fromJust . hex2num . relist)

hex :: BNF.Parser TextState DiffString
hex =
  digit `BNF.or`
  get_any_char1 ['A'..'F'] `BNF.or`
  get_any_char1 ['a'..'f']

number :: BNF.Parser TextState JSValue
number =
  integer `BNF.and` fraction `BNF.and` JSON.exponent >>=
    return . JSNumber . relist

-- NOTE: Variable length matcher _digits_ MUST come before fixed length
--   matcher _digit_. Otherwise, the composed matcher _integer_ will ALWAYS
--   short-circuit on first digit.
integer :: BNF.Parser TextState DiffString
integer =
  (onenine `BNF.and` digits) `BNF.or`
  digit `BNF.or`
  (get_char1 '-' `BNF.and` onenine `BNF.and` digits) `BNF.or`
  (get_char1 '-' `BNF.and` digit)

digits :: BNF.Parser TextState DiffString
digits = BNF.oom (digit)

digit :: BNF.Parser TextState DiffString
digit = get_char1 '0' `BNF.or` onenine

onenine :: BNF.Parser TextState DiffString
onenine = get_any_char1 ['1'..'9']

fraction :: BNF.Parser TextState DiffString
fraction = BNF.zoo (get_char1 '.' `BNF.and` digits)

exponent :: BNF.Parser TextState DiffString
exponent =
  BNF.zoo (
     get_char1 'E' `BNF.and` sign `BNF.and` digits `BNF.or`
    (get_char1 'e' `BNF.and` sign `BNF.and` digits))

sign :: BNF.Parser TextState DiffString
sign = BNF.zoo (get_char1 '+' `BNF.or` get_char1 '-')

-- NOTE: Line breaks are normalized within BNF.Text.
ws :: Monoid a => BNF.Parser TextState a
ws =
  BNF.zom (
    meta_char '\x0020' `BNF.or`
    meta_break         `BNF.or`
    meta_char '\x0009')
