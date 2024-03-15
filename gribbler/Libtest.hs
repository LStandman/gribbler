-- SPDX-License-Identifier: GPL-3.0-or-later
-- Libtest.hs: Unit test infrastructure
-- Copyright (C) 2021-2023 LStandman

module Libtest(
    expect_buffeq,
    expect_false,
    expect_memeq,
    expect_that,
    expect_true,
    runtests,
    test,
    testsuite)
  where

import Data.Array
import System.CPUTime
import Text.Printf
--
import Misc.MemUtils

type Matcher a = a -> IO Bool

expect_false :: Bool -> IO Bool
expect_memeq :: (Eq a, Show a) => String -> a -> a -> IO Bool
expect_that  :: Matcher a -> a -> IO Bool
expect_true  :: Bool -> IO Bool
runtests     :: [IO Bool] -> IO Bool
test         :: String -> [IO Bool] -> IO Bool
testsuite    :: String -> [IO Bool] -> IO Bool

runtests [] = return True
runtests (t:ts) = t >>= \ x -> runtests ts >>= \ y -> return (x && y)

test' :: [IO Bool] -> IO Bool
test' [] = return True
test' (e:es) =
  e >>= \ x -> if not x then return False else test' es

test name es =
  printf "[ RUN      ] %s\n" name >>
    getCPUTime >>= \ start ->
      test' es >>= \ x ->
        getCPUTime >>= \ end ->
          printf "%s %s\n" (if not x then "[  FAILED  ]" else "[       OK ]") name >>
            printf "[       ** ] time: %0.1fms\n" ((fromIntegral (end - start)) / (10^9) :: Double) >>
              return x

testsuite' :: [IO Bool] -> IO Bool
testsuite' [] = return True
testsuite' (t:ts) = t >>= \ x -> testsuite' ts >>= \ y -> return (x && y)

testsuite name tests =
  printf "[----------] tests from %s\n" name >>
    testsuite' tests >>=
      \ x -> printf "[----------]\n" >> return x

expect_that matcher = matcher

buffeq :: Integral a => String -> [a] -> [a] -> IO Bool
buffeq varname expected actual =
  if actual == expected
    then
      return True
    else
      print ("Value of: " ++ varname) >>
        print ("  Actual: " ++ (show $ map (\ x -> "0x" ++ num2hex 2 (fromIntegral x)) actual)) >>
          print ("Expected: " ++ (show $ map (\ x -> "0x" ++ num2hex 2 (fromIntegral x)) expected)) >>
            return False
expect_buffeq varname expected = expect_that (buffeq varname expected)

memeq :: (Eq a, Show a) => String -> a -> a -> IO Bool
memeq varname expected actual =
  if actual == expected
    then
      return True
    else
      print ("Value of: " ++ varname) >>
        print ("  Actual: " ++ (show actual)) >>
          print ("Expected: " ++ (show expected)) >>
            return False

expect_memeq varname expected = expect_that (memeq varname expected)

expect_true = return

expect_false = return . not
