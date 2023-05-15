-- SPDX-License-Identifier: GPL-3.0-or-later
-- YAML.hs: YAML compatible parser
-- Copyright (C) 2021-2023 LStandman

module YAML(
--    construct,
--    present,
--    esc_htab,
--    b_break,
    Context (..),
    b_as_line_feed,
    ns_esc_htab,
    nb_char,
    b_break,
    c_printable,
    ns_uri_char,
    c_ns_esc_char,
    s_indent,
    b_l_folded,
    s_space)
  where

import Data.List
import Data.Maybe
--
import BNF
import BNF.Extras
import MemUtils

data Context = BlockOut | BlockIn | FlowOut | FlowIn

any_char :: [Char] -> Parser String String
any_char = (foldl1 (altr)) . (map (match_char))

start_of_line = nop

-- [0]
c_printable       =
  any_char $
    ['\x09', '\x0A', '\x0D'] ++ ['\x20'..'\x7E'] ++
    ['\x85'] ++ ['\xA0'..'\xD7FF'] ++ ['\xE000'..'\xFFFD'] ++
    ['\x010000'..'\x10FFFF']
nb_json            =
  any_char $ ['\x09'] ++ ['\x20'..'\x10FFFF']
c_byte_order_mark = return Error "Not implemented"

c_sequence_entry = match_char '-'
c_mapping_key    = match_char '?'
c_mapping_value  = match_char ':'
c_collect_entry  = match_char ','
c_sequence_start = match_char '['
c_sequence_end   = match_char ']'
c_mapping_start  = match_char '['
c_mapping_end    = match_char ']'
c_comment        = match_char '#'
c_anchor         = match_char '&'
c_alias          = match_char '*'
c_tag            = match_char '!'
c_literal        = match_char '|'
c_folded         = match_char '>'
c_single_quote   = match_char '\''
c_double_quote   = match_char '"'
-- [20]
c_directive      = match_char '%'
c_reserved       = match_char '@' `altr` match_char '`'

c_indicator =
    c_sequence_entry `altr` c_mapping_key   `altr`
    c_mapping_value  `altr` c_collect_entry `altr`
    c_sequence_start `altr` c_sequence_end  `altr`
    c_mapping_start  `altr` c_mapping_end   `altr`
    c_comment        `altr` c_anchor        `altr`
    c_alias          `altr` c_tag           `altr`
    c_literal        `altr` c_folded        `altr`
    c_single_quote   `altr` c_double_quote  `altr`
    c_directive      `altr` c_reserved

c_flow_indicator =
    c_collect_entry  `altr` c_sequence_start `altr`
    c_sequence_end   `altr` c_mapping_start  `altr`
    c_mapping_end

b_line_feed       = match_char '\x0A'
b_carriage_return = match_char '\x0D'
b_char          = b_line_feed `altr` b_carriage_return
nb_char         = c_printable `exclude` b_char `exclude` c_byte_order_mark

b_break =
  (b_carriage_return `conc` b_line_feed) `altr`
  b_carriage_return `altr` b_line_feed

b_as_line_feed = b_break `conv` return "\x0A"

b_non_content = b_break `conv` return ""

s_space   = match_char '\x20'
s_tab     = match_char '\x09'
s_white   = s_space `altr` s_tab
ns_char = nb_char `exclude` s_white

ns_dec_digit    = any_char ['\x30'..'\x39']
ns_hex_digit    =
  ns_dec_digit `altr`
  (any_char $ ['\x41'..'\x46'] ++ ['\x61'..'\x66'])
ns_ascii_letter =
  any_char $ ['\x41'..'\x5A'] ++ ['\x61'..'\x7A']
ns_word_char    = ns_dec_digit `altr` ns_ascii_letter `altr` match_char '-'

ns_uri_char =
  (match_char '%' `conc` rep 2 ns_hex_digit) `altr`
  ns_word_char `altr`
  any_char [
      '#',  ';', '/', '?', ':', '@', '&', '=',
      '+',  '$', ',', '_', '.', '!', '~', '*',
      '\'', '(', ')', '[', ']']

-- [40]
ns_tag_char = ns_uri_char `exclude` c_tag `exclude` c_flow_indicator

c_escape = match_char '\\' `conv` return ""
ns_esc_null            = match_char '0'    `conv` return "\x00"
ns_esc_bell            = match_char 'a'    `conv` return "\x07"
ns_esc_backspace       = match_char 'b'    `conv` return "\x08"
ns_esc_htab = (match_char 't' `altr` match_char '\x09') `conv` return "\x09"
ns_esc_line_feed       = match_char 'n'    `conv` return "\x0A"
ns_esc_vtab            = match_char 'v'    `conv` return "\x0B"
ns_esc_form_feed       = match_char 'f'    `conv` return "\x0C"
ns_esc_carriage_return = match_char 'r'    `conv` return "\x0D"
ns_esc_escape          = match_char 'e'    `conv` return "\x1B"
ns_esc_space           = match_char '\x20' `conv` return "\x20"
ns_esc_dquote          = match_char '"'    `conv` return "\x22"
ns_esc_slash           = match_char '/'    `conv` return "\x2F"
ns_esc_backslash       = match_char '\\'   `conv` return "\x5C"
ns_esc_nextline        = match_char 'N'    `conv` return "\x85"
ns_esc_nbscpace        = match_char '_'    `conv` return "\xA0"
ns_esc_lseparator      = match_char 'L'    `conv` return "\x2028"
ns_esc_pseparator      = match_char 'P'    `conv` return "\x2029"

hex2char s = [toEnum . fromJust . hex2num $ tail s ]

ns_esc_8bit  = match_char 'x' `conc` rep 2 ns_hex_digit `conv` hex2char

-- [60]
ns_esc_16bit = match_char 'u' `conc` rep 4 ns_hex_digit `conv` hex2char

ns_esc_32bit = match_char 'U' `conc` rep 8 ns_hex_digit `conv` hex2char

c_ns_esc_char =
  c_escape `conc`
  (ns_esc_null      `altr` ns_esc_bell            `altr`
  ns_esc_backspace  `altr` ns_esc_htab            `altr`
  ns_esc_line_feed  `altr` ns_esc_vtab            `altr`
  ns_esc_form_feed  `altr` ns_esc_carriage_return `altr`
  ns_esc_escape     `altr` ns_esc_space           `altr`
  ns_esc_dquote     `altr` ns_esc_slash           `altr`
  ns_esc_backslash  `altr` ns_esc_nextline        `altr`
  ns_esc_nbscpace   `altr` ns_esc_lseparator      `altr`
  ns_esc_pseparator `altr` ns_esc_8bit            `altr`
  ns_esc_16bit      `altr` ns_esc_32bit)

s_indent 0 = nop
s_indent n = s_space `conc` s_indent (n - 1)

s_indent_lt 1 = nop
s_indent_lt n = (s_space `conc` s_indent_lt (n - 1)) `altr` nop

s_indent_le 0 = nop
s_indent_le n = (s_space `conc` s_indent_le (n - 1)) `altr` nop

s_seperate_in_line = one_more s_white `altr` start_of_line

s_line_prefix BlockOut = s_block_line_prefix
s_line_prefix BlockIn  = s_block_line_prefix
s_line_prefix FlowOut  = s_flow_line_prefix
s_line_prefix FlowIn   = s_flow_line_prefix

s_block_line_prefix n = s_indent n
s_flow_line_prefix  n = s_indent n `conc` zero_one s_seperate_in_line

l_empty c n = (s_line_prefix c n `altr` s_indent_lt n) `conc` b_as_line_feed

b_l_trimmed c n = b_non_content `conc` one_more (l_empty c n)

b_as_space = b_break `conv` return "\x20"

b_l_folded :: Context -> Int -> Parser String String
b_l_folded c n = b_l_trimmed c n `altr` b_as_space
