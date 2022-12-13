module Main where

import BNF
import Misc
import YAML

main :: IO ()
main = do
  --print (esc_char "\r\nasdf")
  --print (esc_char "\\xaasdf")
  --print (esc_char "\\nabasdf")
  --print (esc_char "xabasdf")
  print $ fmap (relist) $ match_char 'c' "carl"
  print $ fmap (relist) $ match_char 'c' "xarl"
  print $ fmap (relist) $ zero_more (match_char 'c') "xarl"
  print $ fmap (relist) $ zero_more (match_char 'c') "carl"
  print $ fmap (relist) $ zero_more (match_char 'c') "ccrl"
  print $ fmap (relist) $ one_more (match_char 'c') "xarl"
  print $ fmap (relist) $ one_more (match_char 'c') "carl"
  print $ fmap (relist) $ one_more (match_char 'c') "ccrl"
  print $ fmap (relist) $ zero_one (match_char 'c') "xarl"
  print $ fmap (relist) $ zero_one (match_char 'c') "carl"
  print $ fmap (relist) $ zero_one (match_char 'c') "ccrl"
  print $ fmap (relist) $ b_break "\n\rccrl"
  print $ fmap (relist) $ b_break "\n\nccrl"
  print $ fmap (relist) $ b_break "\r\rccrl"
  print $ fmap (relist) $ b_break "\r\nccrl"
  print $ fmap (relist) $ b_break "ccrl"
  print $ fmap (relist) $ nb_char "ccrl"
  print $ fmap (relist) $ nb_char "\ncrl"
  print $ fmap (relist) $ printable "carl"
  print $ fmap (relist) $ printable "\x85arl"
  print $ fmap (relist) $ uri_char "%AFxxxx"
  print $ fmap (relist) $ uri_char "%AGxxxx"
  print $ fmap (relist) $ as_line_feed "\n\rccrl"
  print $ fmap (relist) $ as_line_feed "\n\nccrl"
  print $ fmap (relist) $ as_line_feed "\r\rccrl"
  print $ fmap (relist) $ as_line_feed "\r\nccrl"
--  print ((Hit 'c') <> Miss)
  print "\x85"
