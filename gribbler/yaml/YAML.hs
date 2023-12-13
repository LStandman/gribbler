-- SPDX-License-Identifier: GPL-3.0-or-later
-- YAML.hs: YAML compatible parser
-- Copyright (C) 2021-2023 LStandman

module YAML(
--    construct,
--    present,
--    esc_htab,
--    b_break,
    Context (..),
    match_char,
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
    s_space,
    c_single_quoted)
  where

import Data.Maybe
import Data.Semigroup
--
import BNF
import DiffList
import MemUtils

data Context = BlockIn | BlockKey | BlockOut | FlowIn | FlowKey | FlowOut
type TextState  = (String, Int, Int)
type TextParser = Parser TextState DiffString
type TextResult = Result TextState DiffString

match_char :: Char -> TextParser
match_char c = \ (xs, n, sol) -> case xs of
  []     -> Miss
  (y:ys) -> case c == y of
    True  -> Hit ((ys, n + 1, sol), difflist [c])
    False -> Miss

match_text :: [Char] -> TextParser
match_text [] = nul
match_text s  = foldl1 (et) $ map (match_char) s

any_char :: [Char] -> TextParser
any_char s = foldl1 (ou) $ map (match_char) s

x_empty = nul :: TextParser

x_start_of_line = \ (xs, n, sol) -> case n == sol of
  True -> Hit ((xs, n, sol), mempty)
  False -> Miss

x_end_of_input = \ (xs, n, sol) -> case xs == [] of
  True -> Hit ((xs, n, sol), mempty)
  False -> Miss

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
nb_char         = c_printable `except` b_char `except` c_byte_order_mark

b_break =
  (b_carriage_return `et` b_line_feed) `ou`
  b_carriage_return `ou` b_line_feed

b_as_line_feed = b_break `finally` \ ((xs, n, _), _) -> ((xs, n, n), difflist "\x0A")

b_non_content = presentation b_break

-- 5.5. White Space Characters
s_space   = match_char '\x20'
s_tab     = match_char '\x09'
s_white   = s_space `ou` s_tab
ns_char = nb_char `except` s_white

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

ns_tag_char = ns_uri_char `except` c_tag `except` c_flow_indicator

-- 6.1. Indentation Spaces
s_indent 0 = nul
s_indent n = s_space `et` s_indent (n - 1)

s_indent_lt 1 = nul
s_indent_lt n = s_space `et` s_indent_lt (n - 1) `ou` nul

s_indent_le 0 = nul
s_indent_le n = s_space `et` s_indent_le (n - 1) `ou` nul

s_indent_get_m :: Parser TextState (Sum Int)
s_indent_get_m = zom (s_space `conv` (return $ Sum 1))

-- 6.2. Separation Spaces
s_separate_in_line = oom s_white `ou` x_start_of_line

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
b_comment = b_non_content `ou` x_end_of_input
s_b_comment =
  zoo (s_separate_in_line `et` zoo c_nb_comment_text) `et`
  b_comment

l_comment = s_separate_in_line `et` zoo c_nb_comment_text `et`
  b_comment

s_l_comments = (s_b_comment `ou` x_start_of_line) `et` zom l_comment

-- 6.7. Separation Lines
s_separate BlockOut n = s_separate_lines n
s_separate BlockIn  n = s_separate_lines n
s_separate FlowOut  n = s_separate_lines n
s_separate FlowIn   n = s_separate_lines n
s_separate BlockKey _ = s_separate_in_line
s_separate FlowKey  _ = s_separate_in_line

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
  (c_verbatim_tag `ou` c_ns_shorthand_tag `ou` c_nul_specific_tag)
  `err` "Node tags are not implemented!"

c_verbatim_tag =
  match_text "!<" `et` oom ns_uri_char `et` match_char '>'

c_ns_shorthand_tag = c_tag_handle `et` oom ns_tag_char

c_nul_specific_tag = match_char '!'

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
c_double_quoted _ _ =
  c_double_quote `err` "Double quoted style is not implemented!"

-- 7.3.2. Single-Quoted Style
c_quoted_quote = match_text "''"

nb_single_char = c_quoted_quote `ou` (nb_json `except` c_single_quote)

ns_single_char = nb_single_char `except` s_white

c_single_quoted c n = c_single_quote `et` nb_single_text c n `et` c_single_quote

nb_single_text FlowOut  n = nb_single_multi_line n
nb_single_text FlowIn   n = nb_single_multi_line n
nb_single_text BlockKey _ = nb_single_one_line
nb_single_text FlowKey  _ = nb_single_one_line

nb_single_one_line = zom nb_single_char

nb_ns_single_in_line = zom (s_white `et` ns_single_char)

s_single_next_line n =
  s_flow_folded n `et`
  zoo (ns_single_char `et` nb_ns_single_in_line `et`
  (s_single_next_line n `ou` zom s_white))

nb_single_multi_line n =
  nb_ns_single_in_line `et` (s_single_next_line n `ou` zom s_white)

-- 7.4. Flow Collection Styles
-- 7.4.1. Flow Sequences
c_flow_sequence _ _ = c_sequence_start `err` "Flow sequences are not implemented!"

-- 7.4.2. Flow Mappings
c_flow_mapping _ _ = c_mapping_start `err` "Flow mappings are not implemented!"

-- 7.5. Flow Nodes
-- WARN: ns-plain cannot be easily deduced to return _unimplemented_ error,
--   error must be emitted at a higher level
ns_flow_yaml_content _ _ = return Miss

c_flow_json_content c n =
  c_flow_sequence c n `ou`
  c_flow_mapping c n `ou`
  c_single_quoted c n `ou`
  c_double_quoted c n

ns_flow_content c n = 
  ns_flow_yaml_content c n `ou`
  c_flow_json_content c n

ns_flow_yaml_node c n =
  c_ns_alias_node `ou`
  ns_flow_yaml_content c n `ou`
  (c_ns_properties c n `et`
    (
      (s_separate c n `et`
      ns_flow_yaml_content c n) `ou`
      e_scalar))

c_flow_json_node c n =
  zoo (c_ns_properties c n `et`
    s_separate c n) `et`
  c_flow_json_content c n

ns_flow_node c n =
  c_ns_alias_node `ou`
  ns_flow_content c n `ou`
  (c_ns_properties c n `et`
    ( (s_separate c n `et`
      ns_flow_content c n) `ou`
    e_scalar))

-- 8.1 Block Scalar Styles
-- 8.1.2. Literal Style
c_l_literal _ = c_literal `err` "Literal style is not implemented!"

-- 8.1.2. Folded Style
c_l_folded _ = c_folded `err` "Folded style is not implemented!"

-- 8.2 Block Collection Styles
-- 8.2.1. Block Sequences

l_block_sequence n =
  oom (s_indent (n + 1) `on_hit` (\ (ctx, _) -> s_indent_get_m ctx) `on_hit`
    \ (ctx, m) -> c_l_block_seq_entry (n + 1 + (getSum m)) ctx)

c_l_block_seq_entry n = c_sequence_entry `look_not_ahead` ns_char `et` s_l_block_indented BlockIn n

s_l_block_indented c n =
  (s_indent_get_m `on_hit` (\ (ctx, m) ->
    ( (ns_l_compact_sequence (n + 1 + (getSum m))) `ou`
      (ns_l_compact_mapping (n + 1 + (getSum m)))) ctx)) `ou`
  s_l_block_node c n `ou`
  (e_node `et` s_l_comments)
