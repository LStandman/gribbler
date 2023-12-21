-- SPDX-License-Identifier: GPL-3.0-or-later
-- Libtest.hs: Unit test infrastructure
-- Copyright (C) 2021-2023 LStandman

module Libtest(
    expect_false,
    expect_memeq,
    expect_that,
    expect_true,
    runtests,
    test,
    testsuite)
  where

import Data.Array

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
  print ("[ RUN      ] " ++ name) >>
    test' es >>=
      \ x ->
        print ((if not x then "[  FAILED  ] " else "[       OK ] ") ++ name) >>
          return x

testsuite' :: [IO Bool] -> IO Bool
testsuite' [] = return True
testsuite' (t:ts) = t >>= \ x -> testsuite' ts >>= \ y -> return (x && y)

testsuite name tests =
  print ("[----------] tests from " ++ name) >>
    testsuite' tests >>=
      \ x -> print "[----------]" >> return x

expect_that matcher = matcher

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
