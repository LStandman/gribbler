-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON.hs: JSON BNF.Parser
-- Copyright (C) 2023-2024 LStandman

module JSON
  ( JSValue (..),
    deserialize,
    jsstring,
    serialize,
  )
where

import Data.Char
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
import GHC.Stack
import qualified JSON.BNF as BNF
import JSON.BNF.Text
import Misc.DiffList
import Misc.MemUtils

data JSValue
  = JSObject [(String, JSValue)]
  | JSArray [JSValue]
  | JSString String
  | JSNumber String
  | JSTrue
  | JSFalse
  | JSNull
  deriving (Eq, Show)

deserialize :: String -> Either String JSValue
jsstring :: String -> Either String JSValue
serialize :: HasCallStack => Bool -> JSValue -> String

padding = 2

isPrintable :: Char -> Bool
isPrintable c = ('\x0020' <= c) && (c <= '\x10FFFF')

value :: BNF.Parser TextState JSValue
value =
  object
    `BNF.or` array
    `BNF.or` (string <&> JSString)
    `BNF.or` (getText "true" >> return JSTrue)
    `BNF.or` (getText "false" >> return JSFalse)
    `BNF.or` (getText "null" >> return JSNull)
    `BNF.or` number

object :: BNF.Parser TextState JSValue
object =
  assertPush (metaChar '{')
    >> (members `BNF.or` ws)
    >>= \m ->
      assertPop "Unterminated braces '{}'" (metaChar '}')
        >> (return . JSObject $ relist m)

members :: BNF.Parser TextState (DiffList (String, JSValue))
members =
  (member <&> (difflist . (: [])))
    `BNF.and` BNF.zoo (metaChar ',' >> members)

member :: BNF.Parser TextState (String, JSValue)
member =
  (ws :: BNF.Parser TextState ())
    >> string
    >>= \s ->
      (ws :: BNF.Parser TextState ())
        >> metaChar ':'
        >> element
        >>= \e -> return (s, e)

array :: BNF.Parser TextState JSValue
array =
  assertPush (metaChar '[')
    >> (elements `BNF.or` ws)
    >>= \e ->
      assertPop "Unterminated brackets '[]'" (metaChar ']')
        >> (return . JSArray $ relist e)

elements :: BNF.Parser TextState (DiffList JSValue)
elements =
  (element <&> (difflist . (: [])))
    `BNF.and` BNF.zoo (metaChar ',' >> elements)

element :: BNF.Parser TextState JSValue
element =
  (ws :: BNF.Parser TextState ())
    >> value
    >>= \v ->
      (ws :: BNF.Parser TextState ())
        >> return v

string :: BNF.Parser TextState String
string =
  assertPush (metaChar '"')
    >> characters
    >>= \s ->
      assertPop "Unterminated string" (metaChar '"')
        >> return (relist s)

characters :: BNF.Parser TextState DiffString
characters = BNF.zom (character <&> (difflist . (: [])))

character :: BNF.Parser TextState Char
character =
  ( assertPush (metaChar '\\')
      >> assertPop "Unsupported escape sequence" escape
  )
    `BNF.or` ( assertNoop "Unsupported character" (getCharWith isPrintable)
                 `BNF.excl` metaChar '"'
             )

escape :: BNF.Parser TextState Char
escape =
  (metaChar '"' >> return '"')
    `BNF.or` (metaChar '\\' >> return '\\')
    `BNF.or` (metaChar 'b' >> return '\b')
    `BNF.or` (metaChar 'f' >> return '\f')
    `BNF.or` (metaChar 'n' >> return '\n')
    `BNF.or` (metaChar 'r' >> return '\r')
    `BNF.or` (metaChar 't' >> return '\t')
    `BNF.or` ( (metaChar 'u' >> BNF.rep 4 hex)
                 <&> (toEnum . fromJust . hex2num . relist)
             )

hex :: BNF.Parser TextState DiffString
hex =
  digit
    `BNF.or` getAnyChar1 ['A' .. 'F']
    `BNF.or` getAnyChar1 ['a' .. 'f']

number :: BNF.Parser TextState JSValue
number =
  (integer `BNF.and` fraction `BNF.and` JSON.exponent)
    <&> (JSNumber . relist)

-- NOTE: Variable length matcher _digits_ MUST come before fixed length
--   matcher _digit_. Otherwise, the composed matcher _integer_ will ALWAYS
--   short-circuit on first digit.
integer :: BNF.Parser TextState DiffString
integer =
  (onenine `BNF.and` digits)
    `BNF.or` digit
    `BNF.or` (getChar1 '-' `BNF.and` onenine `BNF.and` digits)
    `BNF.or` (getChar1 '-' `BNF.and` digit)

digits :: BNF.Parser TextState DiffString
digits = BNF.oom digit

digit :: BNF.Parser TextState DiffString
digit = getChar1 '0' `BNF.or` onenine

onenine :: BNF.Parser TextState DiffString
onenine = getAnyChar1 ['1' .. '9']

fraction :: BNF.Parser TextState DiffString
fraction = BNF.zoo (getChar1 '.' `BNF.and` digits)

exponent :: BNF.Parser TextState DiffString
exponent =
  BNF.zoo
    ( getChar1 'E' `BNF.and` sign `BNF.and` digits
        `BNF.or` (getChar1 'e' `BNF.and` sign `BNF.and` digits)
    )

sign :: BNF.Parser TextState DiffString
sign = BNF.zoo (getChar1 '+' `BNF.or` getChar1 '-')

-- NOTE: Line breaks are normalized within BNF.Text.
ws :: Monoid a => BNF.Parser TextState a
ws =
  BNF.zom
    ( metaChar '\x0020'
        `BNF.or` metaBreak
        `BNF.or` metaChar '\x0009'
    )
    >> BNF.null

deserialize s =
  case BNF.evalParser
    (element >>= \x -> assert_eof >> return x)
    $ textState s of
    BNF.Hit j -> Right j
    BNF.Error e -> Left e
    BNF.Miss -> Left "Unspecified error"
  where
    assert_eof =
      assertNoop
        "JSON is terminated but stream is not empty"
        (metaEof :: BNF.Parser TextState ())

jsstring' :: String -> Either String String
jsstring' = mapM f
  where
    f c
      | c <= '\x10FFFF' = Right c
      | otherwise = Left $ "Unsupported character " ++ show c

jsstring s = JSString <$> jsstring' s

putBr :: (Bool, Int) -> String
putBr (False, _) = ""
putBr (True, n) = "\n" ++ replicate (n * padding) '\x0020'

putString :: HasCallStack => String -> String
putString s =
  "\"" ++ concatMap f s ++ "\""
  where
    f '"' = "\\\""
    f '\\' = "\\\\"
    f '\b' = "\\b"
    f '\f' = "\\f"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f '\t' = "\\t"
    f c
      | isPrintable c = [c]
      | c < '\x0020' = "\\u" ++ (num2hex 4 . fromEnum $ c)
      | otherwise = error ("JSON.putString: unsupported character " ++ show c)

putJson :: HasCallStack => (Bool, Int) -> JSValue -> String
putJson _ JSNull = "null"
putJson _ JSFalse = "false"
putJson _ JSTrue = "true"
putJson _ (JSNumber s) = s
putJson _ (JSString s) = putString s
putJson (pretty, depth) (JSArray v) =
  case v of
    [] -> "[]"
    u ->
      "[" ++ putBr (pretty, deeper)
        ++ intercalate
          ("," ++ putBr (pretty, deeper))
          (map (putJson (pretty, deeper)) u)
        ++ putBr (pretty, depth)
        ++ "]"
  where
    deeper = depth + 1
putJson (pretty, depth) (JSObject v) =
  case v of
    [] -> "{}"
    u ->
      "{" ++ putBr (pretty, deeper)
        ++ intercalate ("," ++ putBr (pretty, deeper)) (map serialize_member u)
        ++ putBr (pretty, depth)
        ++ "}"
  where
    deeper = depth + 1
    serialize_member (name, value) =
      putString name ++ ":" ++ put_ws ++ putJson (pretty, deeper) value
    put_ws
      | pretty = "\x0020"
      | otherwise = ""

serialize pretty = putJson (pretty, 0)
