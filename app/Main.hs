module Main where

import BNF
import YAML

main :: IO ()
main = do
  --print (esc_char "\r\nasdf")
  --print (esc_char "\\xaasdf")
  --print (esc_char "\\nabasdf")
  --print (esc_char "xabasdf")
  print (match_char 'c' "carl")
  print (match_char 'c' "xarl")
  print (zero_more (match_char 'c') "xarl")
  print (zero_more (match_char 'c') "carl")
  print (zero_more (match_char 'c') "ccrl")
  print (one_more (match_char 'c') "xarl")
  print (one_more (match_char 'c') "carl")
  print (one_more (match_char 'c') "ccrl")
  print (zero_one (match_char 'c') "xarl")
  print (zero_one (match_char 'c') "carl")
  print (zero_one (match_char 'c') "ccrl")
  print (b_break "\n\rccrl")
  print (b_break "\n\nccrl")
  print (b_break "\r\rccrl")
  print (b_break "\r\nccrl")
  print (b_break "ccrl")
  print (nb_char "ccrl")
  print (nb_char "\ncrl")
  print (printable "carl")
  print (printable "\x85arl")
  print (uri_char "%AFxxxx")
  print (uri_char "%AGxxxx")
  print (as_line_feed "\n\rccrl")
  print (as_line_feed "\n\nccrl")
  print (as_line_feed "\r\rccrl")
  print (as_line_feed "\r\nccrl")
--  print ((Hit 'c') <> Miss)
  print "\x85"
