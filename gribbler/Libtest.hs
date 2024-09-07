-- SPDX-License-Identifier: GPL-3.0-or-later
-- Libtest.hs: Unit test infrastructure
-- Copyright (C) 2021-2023 LStandman

module Libtest
  ( expect_dbgeq,
    expect_false,
    expect_memeq,
    expect_that,
    expect_true,
    runtests,
    test,
    testsuite,
  )
where

import Data.Array
--
import Misc.MemUtils
import System.CPUTime
import Text.Printf

type Assertion s = s -> IO Bool

expect_false :: Bool -> Assertion s
expect_memeq ::
  (Eq a, Show a) => String -> a -> a -> Assertion (Maybe Int)
expect_that :: (a -> Assertion s) -> a -> Assertion s
expect_true :: Bool -> Assertion s
runtests :: [Assertion ()] -> Assertion ()
test :: String -> [Assertion (Maybe Int)] -> Assertion ()
testsuite :: String -> [Assertion ()] -> Assertion ()

assert_all :: [Assertion ()] -> Assertion ()
assert_all [] = \ _ -> return True
assert_all (t : ts) =
  \ _ -> t () >>= \x -> assert_all ts () >>= \y -> return (x && y)

runtests ts = assert_all ts

test'' :: (Maybe Int) -> [Assertion (Maybe Int)] -> IO Bool
test'' _ [] = return True
test'' ctr (e : es) =
  e ctr >>= \x -> if not x then return False else test'' ctr' es
  where
    ctr' = fmap (+1) ctr

test' :: Bool -> String -> [Assertion (Maybe Int)] -> Assertion ()
test' use_ctr name es =
  \_ ->
  printf "[ RUN      ] %s\n" name
    >> getCPUTime
    >>= \start ->
      test'' ctr es
        >>= \x ->
          getCPUTime
            >>= \end ->
              printf
                "%s %s\n"
                (if not x then "[  FAILED  ]" else "[       OK ]")
                name
                >> printf
                  "[       ** ] time: %0.1fms\n"
                  ((fromIntegral (end - start)) / (10 ^ 9) :: Double)
                >> return x
  where
    ctr = case use_ctr of
            False -> Nothing
            True -> Just 1

test = test' False

testsuite name tests =
  \_ ->
  printf "[----------] tests from %s\n" name
    >> assert_all tests () >>= \x -> printf "[----------]\n" >> return x

expect_that matcher = matcher

dbgeq :: Integral a => String -> [a] -> [a] -> Assertion (Maybe Int)
dbgeq varname expected actual =
  if actual == expected
    then \_ -> return True
    else
      \ctr ->
      print ("Value of: " ++ varname ++ (maybe "" (\ i -> "@" ++ show i) ctr))
        >> print
          ( "  Actual: "
              ++ ( show $
                     map (\x -> "0x" ++ num2hex 2 (fromIntegral x)) actual
                 )
          )
        >> print
          ( "Expected: "
              ++ ( show $
                     map (\x -> "0x" ++ num2hex 2 (fromIntegral x)) expected
                 )
          )
        >> return False

expect_dbgeq varname expected = expect_that (dbgeq varname expected)

memeq :: (Eq a, Show a) => String -> a -> a -> Assertion (Maybe Int)
memeq varname expected actual =
  if actual == expected
    then \_ -> return True
    else
      \ctr ->
      print ("Value of: " ++ varname ++ (maybe "" (\ i -> "@" ++ show i) ctr))
        >> print ("  Actual: " ++ (show actual))
        >> print ("Expected: " ++ (show expected))
        >> return False

expect_memeq varname expected = expect_that (memeq varname expected)

expect_true = return . return

expect_false = return . return . not
