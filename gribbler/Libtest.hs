-- SPDX-License-Identifier: GPL-3.0-or-later
-- Libtest.hs: Unit test infrastructure
-- Copyright (C) 2021-2023 LStandman

module Libtest
  ( expectDbgEq,
    expectFalse,
    expectMemEq,
    expectThat,
    expectTrue,
    runtests,
    test,
    testsuite,
    indexedTest,
  )
where

import Data.Array
--
import Misc.MemUtils
import System.CPUTime
import Text.Printf

type Assertion s = s -> IO Bool

expectFalse :: Bool -> Assertion s
expectMemEq ::
  (Eq a, Show a) => String -> a -> a -> Assertion (Maybe Int)
expectThat :: (a -> Assertion s) -> a -> Assertion s
expectTrue :: Bool -> Assertion s
runtests :: [Assertion ()] -> Assertion ()
test :: String -> [Assertion (Maybe Int)] -> Assertion ()
testsuite :: String -> [Assertion ()] -> Assertion ()
indexedTest :: String -> [Assertion (Maybe Int)] -> Assertion ()
assertAll :: [Assertion ()] -> Assertion ()
assertAll [] = \_ -> return True
assertAll (t : ts) =
  \_ -> t () >>= \x -> assertAll ts () >>= \y -> return (x && y)

runtests = assertAll

run :: Maybe Int -> [Assertion (Maybe Int)] -> IO Bool
run _ [] = return True
run ctr (e : es) =
  e ctr >>= \x ->
    (if x then run (fmap (+ 1) ctr) es else return False)

showRun :: Bool -> String -> [Assertion (Maybe Int)] -> Assertion ()
showRun useCtr name es =
  \_ ->
    printf "[ RUN      ] %s\n" name
      >> getCPUTime
      >>= \start ->
        run ctr es
          >>= \x ->
            getCPUTime
              >>= \end ->
                printf
                  "%s %s\n"
                  (if not x then "[  FAILED  ]" else "[       OK ]")
                  name
                  >> printf
                    "[       ** ] time: %0.1fms\n"
                    (fromIntegral (end - start) / (10 ^ 9) :: Double)
                  >> return x
  where
    ctr
      | useCtr = Just 1
      | otherwise = Nothing

test = showRun False

indexedTest = showRun True

testsuite name tests _ =
  printf "[----------] tests from %s\n" name
    >> assertAll tests () >>= \x -> printf "[----------]\n" >> return x

expectThat matcher = matcher

dbgeq :: Integral a => String -> [a] -> [a] -> Assertion (Maybe Int)
dbgeq varname expected actual ctr =
  if actual == expected
    then return True
    else
      print ("Value of: " ++ varname ++ maybe "" (\i -> "@" ++ show i) ctr)
        >> print
          ( "  Actual: "
              ++ show (map (\x -> "0x" ++ num2hex 2 (fromIntegral x)) actual)
          )
        >> print
          ( "Expected: "
              ++ show (map (\x -> "0x" ++ num2hex 2 (fromIntegral x)) expected)
          )
        >> return False

expectDbgEq varname expected = expectThat (dbgeq varname expected)

memeq :: (Eq a, Show a) => String -> a -> a -> Assertion (Maybe Int)
memeq varname expected actual ctr =
  if actual == expected
    then return True
    else
      print
        ("Value of: " ++ varname ++ maybe "" (\i -> "@" ++ show i) ctr)
        >> print ("  Actual: " ++ show actual)
        >> print ("Expected: " ++ show expected)
        >> return False

expectMemEq varname expected = expectThat (memeq varname expected)

expectTrue = return . return

expectFalse = return . return . not
