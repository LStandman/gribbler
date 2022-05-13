module Main where

import YAML

main :: IO ()
main = do
  --print (esc_char "\r\nasdf")
  --print (esc_char "\\xaasdf")
  --print (esc_char "\\nabasdf")
  --print (esc_char "xabasdf")
  print (match_char 'c' (Load "" "carl"))
  print (match_char 'c' (Load "" "xarl"))
  print (zero_more (match_char 'c') (Load "" "xarl"))
  print (zero_more (match_char 'c') (Load "" "carl"))
  print (zero_more (match_char 'c') (Load "" "ccrl"))
  print (one_more (match_char 'c') (Load "" "xarl"))
  print (one_more (match_char 'c') (Load "" "carl"))
  print (one_more (match_char 'c') (Load "" "ccrl"))
  print (zero_one (match_char 'c') (Load "" "xarl"))
  print (zero_one (match_char 'c') (Load "" "carl"))
  print (zero_one (match_char 'c') (Load "" "ccrl"))
  print (b_break (Load "" "\n\rccrl"))
  print (b_break (Load "" "\n\nccrl"))
  print (b_break (Load "" "\r\rccrl"))
  print (b_break (Load "" "\r\nccrl"))
  print (b_break (Load "" "ccrl"))
  print (nb_char (Load "" "ccrl"))
  print (nb_char (Load "" "\ncrl"))
  print (printable (Load "" "carl"))
  print (printable (Load "" "\x85arl"))
  print "\x85"
