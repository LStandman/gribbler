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
  print $ match_char 'c' $ text "carl"
  print $ match_char 'c' $ text "xarl"
  print $ zero_or_more (match_char 'c') $ text "xarl"
  print $ zero_or_more (match_char 'c') $ text "carl"
  print $ zero_or_more (match_char 'c') $ text "ccrl"
  print $ one_or_more (match_char 'c') $ text "xarl"
  print $ one_or_more (match_char 'c') $ text "carl"
  print $ one_or_more (match_char 'c') $ text "ccrl"
  print $ zero_or_one (match_char 'c') $ text "xarl"
  print $ zero_or_one (match_char 'c') $ text "carl"
  print $ zero_or_one (match_char 'c') $ text "ccrl"
  print $ b_break $ text "\n\rccrl"
  print $ b_break $ text "\n\nccrl"
  print $ b_break $ text "\r\rccrl"
  print $ b_break $ text "\r\nccrl"
  print $ b_break $ text "ccrl"
  print $ nb_char $ text "ccrl"
  print $ nb_char $ text "\ncrl"
  print $ c_printable $ text "carl"
  print $ c_printable $ text "\x85\&arl"
  print $ ns_uri_char $ text "%AFxxxx"
  print $ ns_uri_char $ text "%AGxxxx"
  print $ b_as_line_feed $ text "\n\rccrl"
  print $ b_as_line_feed $ text "\n\nccrl"
  print $ b_as_line_feed $ text "\r\rccrl"
  print $ b_as_line_feed $ text "\r\nccrl"
  print $ c_ns_esc_char $ text "\\\x09"
  print $ c_ns_esc_char $ text "\\U00000041"
  print $ s_indent  1 $ text "   \n\n  as\n  space"
  print $ b_l_folded BlockOut 10 $ text "\n  \n \n\n  as\n  space"
--  print ((Hit 'c') <> Miss)
  print "\x85"
