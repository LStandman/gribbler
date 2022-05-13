-- SPDX-License-Identifier: GPL-3.0-or-later
-- YAML.hs: YAML compatible parser
-- Copyright (C) 2021 LStandman

module YAML(
--    construct,
--    present,
--    esc_htab,
--    b_break,
    Load (..),
    Result (..),
    nb_char,
    b_break,
    printable,
    zero_one,
    zero_more,
    one_more,
    match_char)
  where

import Data.List
import Data.Maybe

infixl 1 `alternate`
infixl 1 `concatenate`
infixl 1 `exclude`
infixl 1 `on_match`
infixl 1 `on_mismatch`
infixl 1 `on_either`

data Context = BlockOut | BlockIn | FlowOut | FlowIn

data Load a =
  Load {
    struct :: a,
    stream :: String}
  deriving (Show)

data Result a =
  Matched    (Load a) |
  Mismatched (Load a) |
  Error      String
  deriving (Show)

type Production a = (Load a -> Result a)

on_match :: Result a -> Production a -> Result a
on_match (Matched a) f = f a
on_match r           _ = r

on_mismatch :: Result a -> Production a -> Result a
on_mismatch (Mismatched a) f = f a
on_mismatch r              _ = r

on_either :: Result a -> (Production a, Production a) -> Result a
on_either (Mismatched a) e = (fst e) a
on_either (Matched a)    e = (snd e) a
on_either r              _ = r

alternate :: Production a -> Production a -> Production a
alternate f g = \ l -> f l `on_mismatch` g

concatenate :: Production a -> Production a -> Production a
concatenate f g = \ l -> f l `on_match` g `on_mismatch` \ _ -> Mismatched l

exclude :: Production a -> Production a -> Production a
exclude f g =
  \ x -> f x `on_match` \ y -> g x `on_either` (\ _ -> Matched y, \ _ -> Mismatched x)

zero_one :: Production a -> Production a
zero_one f = \ l -> f l `on_mismatch` Matched

zero_more :: Production a -> Production a
zero_more f = \ l -> f l `on_either` (Matched, zero_more f)

one_more :: Production a -> Production a
one_more f = \ l -> f l `on_match` zero_more f

match_char :: Char -> Production String
match_char c a
  | c == x    = Matched $ Load ((struct a) ++ [x]) xs
  | otherwise = Mismatched a
  where
    (x:xs) = stream a
    
unimplemented_char :: Char -> Production String
unimplemented_char c a
  | c == x    = Error "Only 8-bit charset is implemented!"
  | otherwise = Mismatched a
  where
    (x:xs) = stream a

printable :: Production String
printable =
  foldl1 (alternate) $
  (map (match_char) $ 
    ['\x09', '\x0A', '\x0D'] ++ ['\x20'..'\x7E']) ++
  (map (unimplemented_char) $ 
    ['\x85'] ++ ['\xA0'..'\xD7FF'] ++
    ['\xE000'..'\xFFFD'] ++ ['\x010000'..'\x10FFFF'])
  
json            = undefined
byte_order_mark = undefined

sequence_entry = match_char '-'
mapping_key    = match_char '?'
mapping_value  = match_char ':'
collect_entry  = match_char ','
sequence_start = match_char '['
sequence_end   = match_char ']'
mapping_start  = match_char '['
mapping_end    = match_char ']'
comment        = match_char '#'
anchor         = match_char '&'
alias          = match_char '*'
tag            = match_char '!'
literal        = match_char '|'
folded         = match_char '>'
single_quote   = match_char '\''
double_quote   = match_char '"'
directive      = match_char '%'
reserved       = (match_char '@') `alternate` (match_char '`')

indicator =
    sequence_entry `alternate` mapping_key   `alternate`
    mapping_value  `alternate` collect_entry `alternate`
    sequence_start `alternate` sequence_end  `alternate`
    mapping_start  `alternate` mapping_end   `alternate`
    comment        `alternate` anchor        `alternate`
    alias          `alternate` tag           `alternate`
    literal        `alternate` folded        `alternate`
    single_quote   `alternate` double_quote  `alternate`
    directive      `alternate` reserved

flow_indicator =
    collect_entry  `alternate` sequence_start `alternate`
    sequence_end   `alternate` mapping_start  `alternate`
    mapping_end

line_feed       = match_char '\x0A'
carriage_return = match_char '\x0D'
b_char          = line_feed `alternate` carriage_return
nb_char         = printable `exclude` b_char

b_break =
  (carriage_return `concatenate` line_feed) `alternate`
  carriage_return `alternate` line_feed

as_line_feed = f . b_break
  where
    f (Error      a) = Error a
    f (Mismatched a) = Mismatched a
    f (Matched    a) = Matched $ Load ((struct a) ++ "\x0A") (stream a)

data Yaml =
  Mapping [(String, Yaml)] |
  Sequence [Yaml] |
  Scalar String |
  Invalid String




