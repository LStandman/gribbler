module Main where

import Control.Exception
import Data.Either
import System.CPUTime
import Text.Printf

import qualified JSON.BNF as BNF
import JSON.BNF.Text
import JSON
import Misc.MemUtils
import qualified Misc.Base64.RFC4648 as Base64

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
  putStrLn $ Base64.encode False $ strBytes "t"
  putStrLn $ Base64.encode False $ strBytes "Many hands make light work."
  putStrLn $ num2hex1. fromEnum $ '\x01'
  putStrLn $ serialize True $ JSArray [JSString "ab", JSString "cd", JSString "ef", JSArray [JSString "01", JSString "02", JSString "03", JSString "04"]]
  putStrLn $ serialize False $ JSArray [JSString "ab", JSString "cd", JSString "ef", JSArray [JSString "01", JSString "02", JSString "03", JSString "04"]]
  putStrLn $ serialize False $ JSString "\x01"
  putStrLn $ serialize False $ JSString "\x001F"
  putStrLn $ serialize True $ JSObject [("alive", JSTrue), ("age", JSNumber "27")]
  putStrLn $ serialize False $ JSObject [("alive", JSTrue), ("age", JSNumber "27")]
  print $ deserialize "[\"Comma after the close\"],"
  print $ deserialize "[{\"age\": 27]"
  putStrLn $ fromLeft "" $ deserialize "{\"alive\": true, \"\x001\": 27}"
  putStrLn $ fromLeft "" $ deserialize "[{\"age\x001\": 27]"
  print $ deserialize "\"test\""
  print $ deserialize "\"\\u0100\""
--  print $ BNF.run_parser string "\"test\""
--  print $ BNF.run_parser (drop_char '"' :: TextParser) "\"test"
  print $ deserialize "[]"
  print $ deserialize "[\
\    \"JSON Test Pattern pass1\",\
\    {\"object with 1 member\":[\"array with 1 element\"]},\
\    {},\
\    []]"
