-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module BNF(
    Parser(..),
    Result(..),
    BNF.and,
    BNF.null,
    BNF.or,
    err,
    evalParser,
    except,
    execParser,
    oom,
    rep,
    runParser,
    zom,
    zoo)
  where

import Control.Monad

infixl 1 `err`
infixl 1 `and`
infixl 1 `or`
infixl 1 `except`

data Result a =
    Hit a        |
    Miss         |
    Error String
  deriving (Eq, Show)

newtype Parser s a = Parser (s -> Result (a, s))

and        :: Semigroup a => Parser s a -> Parser s a -> Parser s a
err        :: Parser s a -> String -> Parser s a
evalParser :: Parser s a -> s -> Result a
except     :: Parser s a -> Parser s a -> Parser s a
execParser :: Parser s a -> s -> Result s
null       :: Monoid a => Parser s a
oom        :: Semigroup a => Parser s a -> Parser s a
or         :: Parser s a -> Parser s a -> Parser s a
rep        :: Semigroup a => Int -> Parser s a -> Parser s a
runParser  :: Parser s a -> (s -> Result (a, s))
zom        :: Monoid a => Parser s a -> Parser s a
zoo        :: Monoid a => Parser s a -> Parser s a

instance Monad Result
  where
    return = Hit
    (Hit h)   >>= f = f h
    Miss      >>= _ = Miss
    (Error e) >>= _ = Error e

instance Applicative Result
  where
    pure  = return
    (<*>) = ap

instance Functor Result
  where
    fmap = liftM

instance Monad (Parser s)
  where
    return x = Parser (\ s -> Hit (x, s))
    (Parser f') >>= g =
      Parser (\ s -> f' s >>= \ (x, s') -> runParser (g x) s')

instance Applicative (Parser s)
  where
    pure  = return
    (<*>) = ap

instance Functor (Parser s)
  where
    fmap = liftM

runParser (Parser f') = f'

evalParser f s = runParser f s >>= return . fst

execParser f s = runParser f s >>= return . snd

or (Parser f') (Parser g') =
  Parser (\ s -> case f' s of
    Miss -> g' s
    r'   -> r')

and f g =
  f >>= \ x -> g >>= \ x' -> return (x <> x')

err' :: String -> Parser s a
err' e = Parser (\ _ -> Error e)

err f e = f >>= \ _ -> err' e

rep 1 f = f
rep n f = f `BNF.and` rep (n - 1) f

except f (Parser g') =
  Parser (\ s -> runParser 
    (f >>= \ x -> case g' s of
      Hit   _ -> Parser (\ _ -> Miss)
      Miss    -> return x
      Error e -> err' e) s)

null = return mempty

zoo f = f `BNF.or` BNF.null

zom f = f `BNF.and` zom f `BNF.or` BNF.null

oom f = f `BNF.and` oom f `BNF.or` f
