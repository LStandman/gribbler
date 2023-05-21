-- SPDX-License-Identifier: GPL-3.0-or-later
-- YAML.hs: YAML compatible parser
-- Copyright (C) 2021-2023 LStandman

module YAML(
--    construct,
--    present,
--    esc_htab,
--    b_break,
    Context (..),
    b_non_content,
    l_empty,
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

import Data.Maybe
--
import BNF
import BNF.Text
import MemUtils

data Context = BlockIn | BlockKey | BlockOut | FlowIn | FlowKey | FlowOut

any_char :: [Char] -> Parser String String
any_char s = foldl1 (ou) $ map (match_char) s

match_text :: [Char] -> Parser String String
match_text [] = non
match_text s  = foldl1 (et) $ map (match_char) s

start_of_line = non
end_of_input = non

presentation :: Parser String String -> Parser String String
presentation f = f `conv` return ""

-- [0]
c_printable       =
  any_char $
    ['\x09', '\x0A', '\x0D'] ++ ['\x20'..'\x7E'] ++
    ['\x85'] ++ ['\xA0'..'\xD7FF'] ++ ['\xE000'..'\xFFFD'] ++
    ['\x010000'..'\x10FFFF']
nb_json            =
  any_char $ ['\x09'] ++ ['\x20'..'\x10FFFF']
c_byte_order_mark = match_char '\xFEFF'

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
c_reserved       = match_char '@' `ou` match_char '`'

c_indicator =
    c_sequence_entry `ou` c_mapping_key   `ou`
    c_mapping_value  `ou` c_collect_entry `ou`
    c_sequence_start `ou` c_sequence_end  `ou`
    c_mapping_start  `ou` c_mapping_end   `ou`
    c_comment        `ou` c_anchor        `ou`
    c_alias          `ou` c_tag           `ou`
    c_literal        `ou` c_folded        `ou`
    c_single_quote   `ou` c_double_quote  `ou`
    c_directive      `ou` c_reserved

c_flow_indicator =
    c_collect_entry  `ou` c_sequence_start `ou`
    c_sequence_end   `ou` c_mapping_start  `ou`
    c_mapping_end

b_line_feed       = match_char '\x0A'
b_carriage_return = match_char '\x0D'
b_char          = b_line_feed `ou` b_carriage_return
nb_char         = c_printable `sauf` b_char `sauf` c_byte_order_mark

b_break =
  (b_carriage_return `et` b_line_feed) `ou`
  b_carriage_return `ou` b_line_feed

b_as_line_feed = b_break `conv` return "\x0A"

b_non_content = presentation b_break

s_space   = match_char '\x20'
s_tab     = match_char '\x09'
s_white   = s_space `ou` s_tab
ns_char = nb_char `sauf` s_white

ns_dec_digit    = any_char ['\x30'..'\x39']
ns_hex_digit    =
  ns_dec_digit `ou`
  (any_char $ ['\x41'..'\x46'] ++ ['\x61'..'\x66'])
ns_ascii_letter =
  any_char $ ['\x41'..'\x5A'] ++ ['\x61'..'\x7A']
ns_word_char    = ns_dec_digit `ou` ns_ascii_letter `ou` match_char '-'

ns_uri_char =
  (match_char '%' `et` rep 2 ns_hex_digit) `ou`
  ns_word_char `ou`
  any_char [
      '#',  ';', '/', '?', ':', '@', '&', '=',
      '+',  '$', ',', '_', '.', '!', '~', '*',
      '\'', '(', ')', '[', ']']

-- [40]
ns_tag_char = ns_uri_char `sauf` c_tag `sauf` c_flow_indicator

c_escape = match_char '\\' `conv` return ""
ns_esc_null            = match_char '0'    `conv` return "\x00"
ns_esc_bell            = match_char 'a'    `conv` return "\x07"
ns_esc_backspace       = match_char 'b'    `conv` return "\x08"
ns_esc_htab = (match_char 't' `ou` match_char '\x09') `conv` return "\x09"
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

ns_esc_8bit  = match_char 'x' `et` rep 2 ns_hex_digit `conv` hex2char

-- [60]
ns_esc_16bit = match_char 'u' `et` rep 4 ns_hex_digit `conv` hex2char

ns_esc_32bit = match_char 'U' `et` rep 8 ns_hex_digit `conv` hex2char

c_ns_esc_char =
  c_escape `et`
  (ns_esc_null      `ou` ns_esc_bell            `ou`
  ns_esc_backspace  `ou` ns_esc_htab            `ou`
  ns_esc_line_feed  `ou` ns_esc_vtab            `ou`
  ns_esc_form_feed  `ou` ns_esc_carriage_return `ou`
  ns_esc_escape     `ou` ns_esc_space           `ou`
  ns_esc_dquote     `ou` ns_esc_slash           `ou`
  ns_esc_backslash  `ou` ns_esc_nextline        `ou`
  ns_esc_nbscpace   `ou` ns_esc_lseparator      `ou`
  ns_esc_pseparator `ou` ns_esc_8bit            `ou`
  ns_esc_16bit      `ou` ns_esc_32bit)

s_indent 0 = non
s_indent n = s_space `et` s_indent (n - 1)

s_indent_lt 1 = non
s_indent_lt n = s_space `et` s_indent_lt (n - 1) `ou` non

s_indent_le 0 = non
s_indent_le n = s_space `et` s_indent_le (n - 1) `ou` non

s_separate_in_line = oom s_white `ou` start_of_line

s_line_prefix BlockOut = s_block_line_prefix
s_line_prefix BlockIn  = s_block_line_prefix
s_line_prefix FlowOut  = s_flow_line_prefix
s_line_prefix FlowIn   = s_flow_line_prefix

s_block_line_prefix  = s_indent
s_flow_line_prefix n = s_indent n `et` zoo s_separate_in_line

l_empty c n = s_line_prefix c n `ou` s_indent_lt n `et` b_as_line_feed

b_l_trimmed c n =
  b_non_content `et` oom (l_empty c n `conv` return "\n")

b_as_space = b_break `conv` return "\x20"

b_l_folded c n = b_l_trimmed c n `ou` b_as_space

s_flow_folded n =
  zoo s_separate_in_line `et`
  b_l_folded FlowIn n `et` s_flow_line_prefix n

c_nb_comment_text = c_comment `et` zom nb_char
b_comment = b_non_content `ou` end_of_input
s_b_comment =
  zoo (s_separate_in_line `et` zoo c_nb_comment_text) `et`
  b_comment

l_comment = s_separate_in_line `et` zoo c_nb_comment_text `et`
  b_comment

s_l_comments = (s_b_comment `ou` start_of_line) `et` zom l_comment

-- [80]
s_separate BlockOut n = s_separate_lines n
s_separate BlockIn  n = s_separate_lines n
s_separate FlowOut  n = s_separate_lines n
s_separate FlowIn   n = s_separate_lines n
s_separate BlockKey n = s_separate_in_line
s_separate FlowKey  n = s_separate_in_line

s_separate_lines n =
  (s_l_comments `et` s_flow_line_prefix n) `ou` s_separate_in_line

l_directive =
  c_directive `et`
  (ns_yaml_directive `ou` ns_tag_directive `ou` ns_reserved_directive) `et`
  s_l_comments

ns_reserved_directive =
  ns_directive_name `et`
  zom (s_separate_in_line `et` ns_directive_parameter)

ns_directive_name      = oom ns_char
ns_directive_parameter = oom ns_char

ns_yaml_directive =
  match_text "YAML" `et` s_separate_in_line `et` ns_yaml_version

ns_yaml_version =
  oom ns_dec_digit `et` match_char '.' `et` oom ns_dec_digit

ns_tag_directive =
  match_text "TAG" `et` s_separate_in_line `et`
  c_tag_handle `et` s_separate_in_line `et` ns_tag_prefix

c_tag_handle =
  c_named_tag_handle `ou` c_secondary_tag_handle `ou` c_primary_tag_handle

c_primary_tag_handle   = match_char '!'
c_secondary_tag_handle = match_text "!!"
c_named_tag_handle     = c_tag `et` oom ns_word_char `et` c_tag

ns_tag_prefix = ns_local_tag_prefix `ou` ns_global_tag_prefix

ns_local_tag_prefix  = c_tag `et` zom ns_uri_char
ns_global_tag_prefix = c_tag `et` zom ns_uri_char

c_ns_properties c n =
  (c_ns_tag_property `et`
  zoo (
    s_separate c n `et` c_ns_anchor_property)) `ou`
  (c_ns_anchor_property `et`
  zoo (
    s_separate c n `et` c_ns_tag_property))

c_ns_tag_property =
  c_verbatim_tag `ou` c_ns_shorthand_tag `ou` c_non_specific_tag

c_verbatim_tag =
  match_text "!<" `et` oom ns_uri_char `et` match_char '>'

c_ns_shorthand_tag = c_tag_handle `et` oom ns_tag_char

-- [100]
c_non_specific_tag = match_char '!'

c_ns_anchor_property = c_anchor `et` ns_anchor_name

ns_anchor_char = ns_char `sauf` c_flow_indicator
ns_anchor_name = oom ns_anchor_char

c_ns_alias_node = c_alias `et` ns_anchor_name

e_scalar = match_text ""
e_node   = e_scalar

nb_double_char =
  c_ns_esc_char `ou` (nb_json `sauf` c_escape `sauf` c_double_quote)

ns_double_char = nb_double_char `sauf` s_white

c_doube_quoted c n = c_double_quote `et` nb_double_text c n `et` c_double_quote

nb_double_text FlowOut  n = nb_double_multi_line n
nb_double_text FlowIn   n = nb_double_multi_line n
nb_double_text BlockKey n = nb_double_one_line
nb_double_text FlowKey  n = nb_double_one_line

nb_double_one_line = zom nb_double_char

s_double_escaped n =
  zom s_white `et` c_escape `et` b_non_content `et`
  l_empty FlowIn n `et` s_flow_line_prefix n

s_double_break n = s_double_escaped n `ou` s_flow_folded n

nb_ns_double_in_line = zom (zom s_white `et` ns_double_char)

s_double_next_line n =
  s_double_break n `et`
  zoo (
    ns_double_char `et` nb_ns_double_in_line `et`
    (s_double_next_line n `et` zom s_white))

nb_double_multi_line n =
  nb_ns_double_in_line `et`
  (s_double_next_line n `ou` zom s_white)

c_quoted_quote = match_text "''"

nb_single_char = c_quoted_quote `ou` (nb_json `sauf` c_single_quote)

ns_single_char = nb_single_char `sauf` s_white

-- [120]
--c_singe_quoted c n = c_single_quote `et` nb_single_text c n `et` c_single_quote

--nb_single_text FlowOut  n = nb_single_multi_line n
--nb_single_text FlowIn   n = nb_single_multi_line n
--nb_single_text BlockKey n = nb_single_one_line
--nb_single_text FlowKey  n = nb_single_one_line

--nb_single_one_line = zom nb_single_char
