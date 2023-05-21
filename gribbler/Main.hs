module Main where

import BNF
import BNF.Text
import YAML

main :: IO ()
main = do
  --print (esc_char "\r\nasdf")
  --print (esc_char "\\xaasdf")
  --print (esc_char "\\nabasdf")
  --print (esc_char "xabasdf")
  print $ match_char 'c' "carl"
  print $ match_char 'c' "xarl"
  print $ zom (match_char 'c') "xarl"
  print $ zom (match_char 'c') "carl"
  print $ zom (match_char 'c') "ccrl"
  print $ oom (match_char 'c') "xarl"
  print $ oom (match_char 'c') "carl"
  print $ oom (match_char 'c') "ccrl"
  print $ zoo (match_char 'c') "xarl"
  print $ zoo (match_char 'c') "carl"
  print $ zoo (match_char 'c') "ccrl"
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
