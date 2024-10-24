-- SPDX-License-Identifier: GPL-3.0-or-later
-- JSON/BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2024 LStandman

module JSON.BNF
  ( Parser (..),
    Result (..),
    JSON.BNF.and,
    JSON.BNF.null,
    JSON.BNF.or,
    assert,
    err,
    evalParser,
    excl,
    execParser,
    miss,
    oom,
    rep,
    runParser,
    zom,
    zoo,
  )
where

import Control.Monad
import Data.Functor ((<&>))
import GHC.Stack

infixl 1 `err`

infixl 1 `and`

infixl 1 `or`

infixl 1 `excl`

data Result a
  = Hit a
  | Miss
  | Error String
  deriving (Eq, Show)

newtype Parser s a = Parser (s -> Result (a, s))

assert :: String -> Parser s a -> Parser s a
and :: Semigroup a => Parser s a -> Parser s a -> Parser s a
evalParser :: Parser s a -> s -> Result a
excl :: Parser s a -> Parser s b -> Parser s a
execParser :: Parser s a -> s -> Result s
null :: Monoid a => Parser s a
oom :: Semigroup a => Parser s a -> Parser s a
or :: Parser s a -> Parser s a -> Parser s a
rep :: Semigroup a => Int -> Parser s a -> Parser s a
runParser :: Parser s a -> (s -> Result (a, s))
zom :: Monoid a => Parser s a -> Parser s a
zoo :: Monoid a => Parser s a -> Parser s a

instance Monad Result where
  (Hit h) >>= f = f h
  Miss >>= _ = Miss
  (Error e) >>= _ = Error e

instance Applicative Result where
  pure = Hit
  (<*>) = ap

instance Functor Result where
  fmap = liftM

instance Monad (Parser s) where
  (Parser f') >>= g =
    Parser (f' >=> (\(x, s') -> runParser (g x) s'))

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Hit (x, s)
  (<*>) = ap

instance Functor (Parser s) where
  fmap = liftM

runParser (Parser f') = f'

evalParser f s = runParser f s <&> fst

execParser f s = runParser f s <&> snd

or (Parser f') g =
  Parser $
    \s ->
      case f' s of
        Miss -> runParser g s
        r -> r

and f g =
  f >>= \x -> g >>= \x' -> return (x <> x')

err :: String -> Parser s a
err e = Parser . return $ Error e

miss :: Parser s a
miss = Parser $ return Miss

rep 1 f = f
rep n f = f `JSON.BNF.and` rep (n - 1) f

excl f g =
  Parser $
    \s ->
      runParser
        ( f
            >>= \x ->
              case runParser g s of
                Hit _ -> miss
                Miss -> return x
                Error e -> err e
        )
        s

assert e1 (Parser f') =
  Parser $
    \s ->
      case f' s of
        Hit x -> return x
        Miss -> Error e1
        Error e2 -> Error e2

null = return mempty

zoo f = f `JSON.BNF.or` JSON.BNF.null

zom f = f `JSON.BNF.and` zom f `JSON.BNF.or` JSON.BNF.null

oom f = f `JSON.BNF.and` oom f `JSON.BNF.or` f
