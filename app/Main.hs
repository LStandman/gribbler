module Main where

import BNF
import YAML

main :: IO ()
main = do
  --print (esc_char "\r\nasdf")
  --print (esc_char "\\xaasdf")
  --print (esc_char "\\nabasdf")
  --print (esc_char "xabasdf")
  print $ fmap (to_list) $ match_char 'c' "carl"
  print $ fmap (to_list) $ match_char 'c' "xarl"
  print $ fmap (to_list) $ zero_more (match_char 'c') "xarl"
  print $ fmap (to_list) $ zero_more (match_char 'c') "carl"
  print $ fmap (to_list) $ zero_more (match_char 'c') "ccrl"
  print $ fmap (to_list) $ one_more (match_char 'c') "xarl"
  print $ fmap (to_list) $ one_more (match_char 'c') "carl"
  print $ fmap (to_list) $ one_more (match_char 'c') "ccrl"
  print $ fmap (to_list) $ zero_one (match_char 'c') "xarl"
  print $ fmap (to_list) $ zero_one (match_char 'c') "carl"
  print $ fmap (to_list) $ zero_one (match_char 'c') "ccrl"
  print $ fmap (to_list) $ b_break "\n\rccrl"
  print $ fmap (to_list) $ b_break "\n\nccrl"
  print $ fmap (to_list) $ b_break "\r\rccrl"
  print $ fmap (to_list) $ b_break "\r\nccrl"
  print $ fmap (to_list) $ b_break "ccrl"
  print $ fmap (to_list) $ nb_char "ccrl"
  print $ fmap (to_list) $ nb_char "\ncrl"
  print $ fmap (to_list) $ printable "carl"
  print $ fmap (to_list) $ printable "\x85arl"
  print $ fmap (to_list) $ uri_char "%AFxxxx"
  print $ fmap (to_list) $ uri_char "%AGxxxx"
  print $ fmap (to_list) $ as_line_feed "\n\rccrl"
  print $ fmap (to_list) $ as_line_feed "\n\nccrl"
  print $ fmap (to_list) $ as_line_feed "\r\rccrl"
  print $ fmap (to_list) $ as_line_feed "\r\nccrl"
--  print ((Hit 'c') <> Miss)
  print "\x85"
