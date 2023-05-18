module Main where

import BNF
import BNF.Extras
import YAML

main :: IO ()
main = do
  --print (esc_char "\r\nasdf")
  --print (esc_char "\\xaasdf")
  --print (esc_char "\\nabasdf")
  --print (esc_char "xabasdf")
  print $ match_char 'c' "carl"
  print $ match_char 'c' "xarl"
  print $ zero_more (match_char 'c') "xarl"
  print $ zero_more (match_char 'c') "carl"
  print $ zero_more (match_char 'c') "ccrl"
  print $ one_more (match_char 'c') "xarl"
  print $ one_more (match_char 'c') "carl"
  print $ one_more (match_char 'c') "ccrl"
  print $ zero_one (match_char 'c') "xarl"
  print $ zero_one (match_char 'c') "carl"
  print $ zero_one (match_char 'c') "ccrl"
  print $ b_break "\n\rccrl"
  print $ b_break "\n\nccrl"
  print $ b_break "\r\rccrl"
  print $ b_break "\r\nccrl"
  print $ b_break "ccrl"
  print $ nb_char "ccrl"
  print $ nb_char "\ncrl"
  print $ c_printable "carl"
  print $ c_printable "\x85\&arl"
  print $ ns_uri_char "%AFxxxx"
  print $ ns_uri_char "%AGxxxx"
  print $ b_as_line_feed "\n\rccrl"
  print $ b_as_line_feed "\n\nccrl"
  print $ b_as_line_feed "\r\rccrl"
  print $ b_as_line_feed "\r\nccrl"
  print $ c_ns_esc_char "\\\x09"
  print $ c_ns_esc_char "\\U00000041"
  print $ s_indent  1 "   \n\n  as\n  space"
  print $ b_l_folded BlockOut 10 "\n  \n \n\n  as\n  space"
--  print ((Hit 'c') <> Miss)
  print "\x85"
