module Main where

import qualified BNF as BNF
import BNF.Text
import JSON

main :: IO ()
main = do
  print $ json "{\"test\": \"abc\"}"
  print $ json "\"test\""
  print $ BNF.run_parser string "\"test\""
  print $ BNF.run_parser (drop_char '"' :: TextParser) "\"test"
