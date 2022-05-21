-- SPDX-License-Identifier: GPL-3.0-or-later
-- YAML.hs: YAML compatible parser
-- Copyright (C) 2021-2022 LStandman

module YAML(
--    construct,
--    present,
--    esc_htab,
--    b_break,
    as_line_feed,
    nb_char,
    b_break,
    printable,
    uri_char,
    match_char,
    to_list)
  where

import Data.List
import Data.Maybe
import Data.Monoid
--
import BNF

data Context = BlockOut | BlockIn | FlowOut | FlowIn

type DiffList a = Endo ([a])

difflist :: [a] -> DiffList a
difflist l = Endo (l ++)

to_list :: DiffList a -> [a]
to_list d = appEndo d []

match_char :: Char -> Production (DiffList Char)
match_char c = \ (x:xs) -> case c == x of
  True  -> Hit (difflist [x]) xs
  False -> Miss

any_char :: [Char] -> Production (DiffList Char)
any_char = (foldl1 (altr)) . (map (match_char))

printable       =
  any_char $
    ['\x09', '\x0A', '\x0D'] ++ ['\x20'..'\x7E'] ++
    ['\x85'] ++ ['\xA0'..'\xD7FF'] ++ ['\xE000'..'\xFFFD'] ++
    ['\x010000'..'\x10FFFF']
json            =
  any_char $ ['\x09'] ++ ['\x20'..'\x10FFFF']
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
reserved       = (match_char '@') `altr` (match_char '`')

indicator =
    sequence_entry `altr` mapping_key   `altr`
    mapping_value  `altr` collect_entry `altr`
    sequence_start `altr` sequence_end  `altr`
    mapping_start  `altr` mapping_end   `altr`
    comment        `altr` anchor        `altr`
    alias          `altr` tag           `altr`
    literal        `altr` folded        `altr`
    single_quote   `altr` double_quote  `altr`
    directive      `altr` reserved

flow_indicator =
    collect_entry  `altr` sequence_start `altr`
    sequence_end   `altr` mapping_start  `altr`
    mapping_end

line_feed       = match_char '\x0A'
carriage_return = match_char '\x0D'
b_char          = line_feed `altr` carriage_return
nb_char         = printable `exclude` b_char

b_break =
  (carriage_return `conc` line_feed) `altr`
  carriage_return `altr` line_feed

as_line_feed =
  b_break `finally` (return $ difflist "\x0A")

non_content = b_break

space   = match_char '\x20'
tab     = match_char '\x09'
white   = space `altr` tab
ns_char = nb_char `exclude` white

dec_digit    = any_char ['\x30'..'\x39']
hex_digit    =
  dec_digit `altr`
  (any_char $ ['\x41'..'\x46'] ++ ['\x61'..'\x66'])
ascii_letter =
  any_char $ ['\x41'..'\x5A'] ++ ['\x61'..'\x7A']
word_char    = dec_digit `altr` ascii_letter `altr` (match_char '-')

uri_char =
  ((match_char '%') `conc` (rep 2 hex_digit)) `altr`
  word_char `altr`
  (any_char [
      '#',  ';', '/', '?', ':', '@', '&', '=',
      '+',  '$', ',', '_', '.', '!', '~', '*',
      '\'', '(', ')', '[', ']'])
tag_char = uri_char `exclude` tag `exclude` flow_indicator

escape              = match_char '\\'
esc_null            = match_char '0'
esc_bell            = match_char 'a'
esc_backspace       = match_char 'b'
esc_htab            = (match_char 't') `altr` (match_char '\x09')
esc_line_feed       = match_char 'n'
esc_vtab            = match_char 'v'
esc_form_feed       = match_char 'f'
esc_carriage_return = match_char 'r'
esc_escape          = match_char 'e'
esc_space           = match_char '\x20'
esc_dquote          = match_char '"'
esc_slash           = match_char '/'
esc_backslash       = match_char '\\'
esc_nextline        = match_char 'N'
esc_nbscpace        = match_char '_'
esc_lseparator      = match_char 'L'
esc_pseparator      = match_char 'P'
