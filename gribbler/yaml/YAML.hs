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

any_char :: [Char] -> TextParser
any_char s = foldl1 (ou) $ map (match_char) s

x_empty = non :: TextParser
start_of_line = non
end_of_input = non

presentation :: TextParser -> TextParser
presentation f = f `conv` (return $ difflist "")

-- 5. Character Productions
-- 5.1. Character Set
c_printable       =
  any_char $
    ['\x09', '\x0A', '\x0D'] ++ ['\x20'..'\x7E'] ++
    ['\x85'] ++ ['\xA0'..'\xD7FF'] ++ ['\xE000'..'\xFFFD'] ++
    ['\x010000'..'\x10FFFF']
nb_json            =
  any_char $ ['\x09'] ++ ['\x20'..'\x10FFFF']

-- 5.2. Character Encodings
c_byte_order_mark = match_char '\xFEFF'

-- 5.3. Indicator Characters
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

-- 5.4. Line Break Characters
b_line_feed       = match_char '\x0A'
b_carriage_return = match_char '\x0D'
b_char          = b_line_feed `ou` b_carriage_return
nb_char         = c_printable `sauf` b_char `sauf` c_byte_order_mark

b_break =
  (b_carriage_return `et` b_line_feed) `ou`
  b_carriage_return `ou` b_line_feed

b_as_line_feed = b_break `conv` (return $ difflist "\x0A")

b_non_content = presentation b_break

-- 5.5. White Space Characters
s_space   = match_char '\x20'
s_tab     = match_char '\x09'
s_white   = s_space `ou` s_tab
ns_char = nb_char `sauf` s_white

-- 5.6. Miscellaneous Characters
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

ns_tag_char = ns_uri_char `sauf` c_tag `sauf` c_flow_indicator

-- 5.7. Escaped Characters
c_escape = match_char '\\' `conv` (return $ difflist "")
ns_esc_null            = match_char '0'    `conv` (return $ difflist "\x00")
ns_esc_bell            = match_char 'a'    `conv` (return $ difflist "\x07")
ns_esc_backspace       = match_char 'b'    `conv` (return $ difflist "\x08")
ns_esc_htab = (match_char 't' `ou` match_char '\x09') `conv` (return $ difflist "\x09")
ns_esc_line_feed       = match_char 'n'    `conv` (return $ difflist "\x0A")
ns_esc_vtab            = match_char 'v'    `conv` (return $ difflist "\x0B")
ns_esc_form_feed       = match_char 'f'    `conv` (return $ difflist "\x0C")
ns_esc_carriage_return = match_char 'r'    `conv` (return $ difflist "\x0D")
ns_esc_escape          = match_char 'e'    `conv` (return $ difflist "\x1B")
ns_esc_space           = match_char '\x20' `conv` (return $ difflist "\x20")
ns_esc_dquote          = match_char '"'    `conv` (return $ difflist "\x22")
ns_esc_slash           = match_char '/'    `conv` (return $ difflist "\x2F")
ns_esc_backslash       = match_char '\\'   `conv` (return $ difflist "\x5C")
ns_esc_nextline        = match_char 'N'    `conv` (return $ difflist "\x85")
ns_esc_nbscpace        = match_char '_'    `conv` (return $ difflist "\xA0")
ns_esc_lseparator      = match_char 'L'    `conv` (return $ difflist "\x2028")
ns_esc_pseparator      = match_char 'P'    `conv` (return $ difflist "\x2029")

hex2char s = difflist [toEnum . fromJust . hex2num $ tail $ relist s ]

ns_esc_8bit  = match_char 'x' `et` rep 2 ns_hex_digit `conv` hex2char

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

-- 6.1. Indentation Spaces
s_indent 0 = non
s_indent n = s_space `et` s_indent (n - 1)

s_indent_lt 1 = non
s_indent_lt n = s_space `et` s_indent_lt (n - 1) `ou` non

s_indent_le 0 = non
s_indent_le n = s_space `et` s_indent_le (n - 1) `ou` non

-- 6.2. Separation Spaces
s_separate_in_line = oom s_white `ou` start_of_line

-- 6.3. Line Prefixes
s_line_prefix BlockOut = s_block_line_prefix
s_line_prefix BlockIn  = s_block_line_prefix
s_line_prefix FlowOut  = s_flow_line_prefix
s_line_prefix FlowIn   = s_flow_line_prefix

s_block_line_prefix  = s_indent
s_flow_line_prefix n = s_indent n `et` zoo s_separate_in_line

-- 6.4. Empty Lines
l_empty c n = s_line_prefix c n `ou` s_indent_lt n `et` b_as_line_feed

-- 6.5. Line Folding
b_l_trimmed c n =
  b_non_content `et` oom (l_empty c n `conv` (return $ difflist "\n"))

b_as_space = b_break `conv` (return $ difflist "\x20")

b_l_folded c n = b_l_trimmed c n `ou` b_as_space

s_flow_folded n =
  zoo s_separate_in_line `et`
  b_l_folded FlowIn n `et` s_flow_line_prefix n

-- 6.6. Comments
c_nb_comment_text = c_comment `et` zom nb_char
b_comment = b_non_content `ou` end_of_input
s_b_comment =
  zoo (s_separate_in_line `et` zoo c_nb_comment_text) `et`
  b_comment

l_comment = s_separate_in_line `et` zoo c_nb_comment_text `et`
  b_comment

s_l_comments = (s_b_comment `ou` start_of_line) `et` zom l_comment

-- 6.7. Separation Lines
s_separate BlockOut n = s_separate_lines n
s_separate BlockIn  n = s_separate_lines n
s_separate FlowOut  n = s_separate_lines n
s_separate FlowIn   n = s_separate_lines n
s_separate BlockKey n = s_separate_in_line
s_separate FlowKey  n = s_separate_in_line

s_separate_lines n =
  (s_l_comments `et` s_flow_line_prefix n) `ou` s_separate_in_line

-- 6.8. Directives
l_directive =
  c_directive `err` "Directives are not implemented!"

c_tag_handle =
  c_named_tag_handle `ou` c_secondary_tag_handle `ou` c_primary_tag_handle

c_primary_tag_handle   = match_char '!'
c_secondary_tag_handle = match_text "!!"
c_named_tag_handle     = c_tag `et` oom ns_word_char `et` c_tag

-- 6.9. Node Properties
c_ns_properties c n =
  (c_ns_tag_property `et`
  zoo (
    s_separate c n `et` c_ns_anchor_property)) `ou`
  (c_ns_anchor_property `et`
  zoo (
    s_separate c n `et` c_ns_tag_property))

c_ns_tag_property =
  (c_verbatim_tag `ou` c_ns_shorthand_tag `ou` c_non_specific_tag)
  `err` "Node tags are not implemented!"

c_verbatim_tag =
  match_text "!<" `et` oom ns_uri_char `et` match_char '>'

c_ns_shorthand_tag = c_tag_handle `et` oom ns_tag_char

c_non_specific_tag = match_char '!'

c_ns_anchor_property =
  c_anchor `err` "Node anchors are not implemented!"

-- 7. Flow Style Productions
-- 7.1. Alias Nodes
c_ns_alias_node = c_alias `err` "Alias nodes are not implemented!"

-- 7.2. Empty Nodes
e_scalar = match_text ""
e_node   = e_scalar

-- 7.3. Flow Scalar Styles
-- 7.3.1. Double-Quoted Style
c_double_quoted c n =
  c_double_quote `err` "Double quoted style is not implemented!"

-- 7.3.2. Single-Quoted Style
c_quoted_quote = match_text "''"

nb_single_char = c_quoted_quote `ou` (nb_json `sauf` c_single_quote)

ns_single_char = nb_single_char `sauf` s_white

c_single_quoted c n = c_single_quote `et` nb_single_text c n `et` c_single_quote

nb_single_text FlowOut  n = nb_single_multi_line n
nb_single_text FlowIn   n = nb_single_multi_line n
nb_single_text BlockKey n = nb_single_one_line
nb_single_text FlowKey  n = nb_single_one_line

nb_single_one_line = zom nb_single_char

nb_ns_single_in_line = zom (s_white `et` ns_single_char)

s_single_next_line n =
  s_flow_folded n `et`
  zoo (ns_single_char `et` nb_ns_single_in_line `et`
  (s_single_next_line n `ou` zom s_white))

nb_single_multi_line n =
  nb_ns_single_in_line `et` (s_single_next_line n `ou` zom s_white)
