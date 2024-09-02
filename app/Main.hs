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
import qualified Crypt.Curve25519 as Curve25519

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
  print $ Curve25519.decode_scalar [
      0xA5, 0x46, 0xE3, 0x6B, 0xF0, 0x52, 0x7C, 0x9D,
      0x3B, 0x16, 0x15, 0x4B, 0x82, 0x46, 0x5E, 0xDD,
      0x62, 0x14, 0x4C, 0x0A, 0xC1, 0xFC, 0x5A, 0x18,
      0x50, 0x6A, 0x22, 0x44, 0xBA, 0x44, 0x9A, 0xC4]
  print $ Curve25519.decode_u_coord [
      0xE6, 0xDB, 0x68, 0x67, 0x58, 0x30, 0x30, 0xDB,
      0x35, 0x94, 0xC1, 0xA4, 0x24, 0xB1, 0x5F, 0x7C,
      0x72, 0x66, 0x24, 0xEC, 0x26, 0xB3, 0x35, 0x3B,
      0x10, 0xA9, 0x03, 0xA6, 0xD0, 0xAB, 0x1C, 0x4C]
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
