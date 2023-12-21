module Main where

import qualified JSON.BNF as BNF
import JSON.BNF.Text
import JSON
import Text.Printf
import Control.Exception
import System.CPUTime

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main :: IO ()
main = do
  print $ json "[{\"age\": 27]"
  print $ json "\"test\""
  print $ json "\"\\u0100\""
--  print $ BNF.run_parser string "\"test\""
--  print $ BNF.run_parser (drop_char '"' :: TextParser) "\"test"
  print $ json "{\
\  \"first_name\": \"John\",\
\  \"last_name\": \"Smith\",\
\  \"is_alive\": true,\
\  \"age\": 27}"
  print $ json "{\
\  \"first_name\": \"John\",\
\  \"last_name\": \"Smith\",\
\  \"is_alive\": true,\
\  \"age\": 27,\
\  \"address\": {\
\    \"street_address\": \"21 2nd Street\",\
\    \"city\": \"New York\",\
\    \"state\": \"NY\",\
\    \"postal_code\": \"10021-3100\"\
\  },\
\  \"phone_numbers\": [\
\    {\
\      \"type\": \"home\",\
\      \"number\": \"212 555-1234\"\
\    },\
\    {\
\      \"type\": \"office\",\
\      \"number\": \"646 555-4567\"\
\    }\
\  ],\
\  \"children\": [\
\    \"Catherine\",\
\    \"Thomas\",\
\    \"Trevor\"\
\  ],\
\  \"spouse\": null\
\}"
