module Main where

import qualified JSON.BNF as BNF
import JSON.BNF.Text
import JSON

main :: IO ()
main = do
  print $ json "{\"test\": \"abc\"}"
  print $ json "\"test\""
  print $ json "\"\\u0100\""
  print $ BNF.run_parser string "\"test\""
  print $ BNF.run_parser (drop_char '"' :: TextParser) "\"test"
