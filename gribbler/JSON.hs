-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023-2024 LStandman

module JSON(
    JSValue(..),
    deserialize,
    jsstring,
    serialize)
  where

import Data.Char
import Data.List
import Data.Maybe
import GHC.Stack

import qualified JSON.BNF as BNF
import JSON.BNF.Text
import Misc.DiffList
import Misc.MemUtils

data JSValue =
    JSObject [(String, JSValue)] |
    JSArray  [JSValue]           |
    JSString String              |
    JSNumber String              |
    JSTrue                       |
    JSFalse                      |
    JSNull
  deriving (Eq, Show)

deserialize :: String -> Either String JSValue
jsstring    :: String -> Either String JSValue
serialize   :: HasCallStack => Bool -> JSValue -> String

padding = 2

is_printable :: Char -> Bool
is_printable c = ('\x0020' <= c) && (c <= '\x10FFFF')

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
  (assert_push $ meta_char '{') >>
    (members `BNF.or` ws) >>=
      \ m -> (assert_pop "Unterminated braces '{}'" $ meta_char '}') >>
        (return . JSObject $ relist m)

members :: BNF.Parser TextState (DiffList (String, JSValue))
members =
  (member >>= return . difflist . (:[])) `BNF.and`
    BNF.zoo (meta_char ',' >> members)

member :: BNF.Parser TextState (String, JSValue)
member =
  (ws :: BNF.Parser TextState ()) >>
    string >>=
      \ s -> (ws :: BNF.Parser TextState ()) >>
        meta_char ':' >>
          element >>=
            \ e -> return (s, e)

array :: BNF.Parser TextState JSValue
array =
  (assert_push $ meta_char '[') >>
    (elements `BNF.or` ws) >>=
      \ e -> (assert_pop "Unterminated brackets '[]'" $ meta_char ']') >>
        (return . JSArray $ relist e)

elements :: BNF.Parser TextState (DiffList JSValue)
elements =
  (element >>= return . difflist. (:[])) `BNF.and`
    BNF.zoo (meta_char ',' >> elements)

element :: BNF.Parser TextState JSValue
element =
  (ws :: BNF.Parser TextState ()) >>
    value >>= \ v ->
      (ws :: BNF.Parser TextState ()) >>
        return v

string :: BNF.Parser TextState String
string =
  (assert_push $ meta_char '"') >>
    characters >>= \ s ->
      (assert_pop "Unterminated string" $ meta_char '"') >>
        (return $ relist s)

characters :: BNF.Parser TextState DiffString
characters = BNF.zom (character >>= return . difflist . (:[]))

character :: BNF.Parser TextState Char
character =
  ( (assert_push $ meta_char '\\') >>
    assert_pop "Unsupported escape sequence" escape) `BNF.or`
  ( (assert_noop "Unsupported character" $ get_char_with (is_printable)) `BNF.excl`
    meta_char '"')

escape :: BNF.Parser TextState Char
escape =
  (meta_char '"'  >> (return '"'))  `BNF.or`
  (meta_char '\\' >> (return '\\')) `BNF.or`
  (meta_char 'b'  >> (return '\b')) `BNF.or`
  (meta_char 'f'  >> (return '\f')) `BNF.or`
  (meta_char 'n'  >> (return '\n')) `BNF.or`
  (meta_char 'r'  >> (return '\r')) `BNF.or`
  (meta_char 't'  >> (return '\t')) `BNF.or`
  (meta_char 'u'  >> BNF.rep 4 hex >>=
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
digits = BNF.oom digit

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
    meta_char '\x0009') >> BNF.null

deserialize s =
  case BNF.eval_parser
    (element >>= \ x -> assert_eof >> return x) $ text_state s of
      BNF.Hit   j -> Right j
      BNF.Error e -> Left e
      BNF.Miss    -> Left "Unspecified error"
  where
    assert_eof =
      assert_noop
        "JSON is terminated but stream is not empty"
        (meta_eof :: BNF.Parser TextState ())

jsstring' :: String -> Either String String
jsstring' s = mapM (f) s
  where
    f c
      | c <= '\x10FFFF' = Right c
      | otherwise = Left $ "Unsupported character " ++ show c

jsstring s = fmap (JSString) $ jsstring' s

br :: (Bool, Int) -> String
br (False, _) = ""
br (True,  n) = "\n" ++ (take (n * padding) $ repeat '\x0020')

serialize_string :: HasCallStack => String -> String
serialize_string s =
  "\"" ++ concatMap (f) s ++ "\""
    where
      f '"'  = "\\\""
      f '\\' = "\\\\"
      f '\b' = "\\b"
      f '\f' = "\\f"
      f '\n' = "\\n"
      f '\r' = "\\r"
      f '\t' = "\\t"
      f c
        | is_printable c = [c]
        | c < '\x0020'   = "\\u" ++ (num2hex 4 . fromEnum $ c)
        | otherwise      = error $ "Unsupported character " ++ show c

serialize' :: HasCallStack => (Bool, Int) -> JSValue -> String
serialize' _ JSNull       = "null"
serialize' _ JSFalse      = "false"
serialize' _ JSTrue       = "true"
serialize' _ (JSNumber s) = s
serialize' _ (JSString s) = serialize_string s

serialize' (pretty, depth) (JSArray v) =
  case v of
    [] -> "[]"
    u  -> "[" ++ br (pretty, deeper) ++
            ( intercalate ("," ++ br (pretty, deeper)) $
              map (serialize' (pretty, deeper)) u) ++
            br (pretty, depth) ++
            "]"
  where
    deeper = depth + 1

serialize' (pretty, depth) (JSObject v) =
  case v of
    [] -> "{}"
    u  -> "{" ++ br (pretty, deeper) ++
            ( intercalate ("," ++ br (pretty, deeper)) $
              map (serialize_member) u) ++
            br (pretty, depth) ++
            "}"
  where
    deeper = depth + 1
    serialize_member (name, value) =
      serialize_string name ++ ":" ++ ws' ++ serialize' (pretty, deeper) value
    ws'
      | pretty    = "\x0020"
      | otherwise = ""

serialize pretty js = serialize' (pretty, 0) js
