-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module BNF(
    Parser,
    Result (..),
    BNF.and,
    BNF.null,
    BNF.or,
    conv,
    err,
    except,
    finally,
    on_hit,
    oom,
    rep,
    zom,
    zoo)
  where

infixl 1 `conv`
infixl 1 `err`
infixl 1 `and`
infixl 1 `finally`
infixl 1 `on_hit`
infixl 1 `on_hit'`
infixl 1 `on_miss`
infixl 1 `or`
infixl 1 `except`

data Result s a =
    Hit (a, s)   |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser s a = s -> Result s a

and     :: Semigroup a => Parser s a -> Parser s a -> Parser s a
conv    :: Parser s a -> (a -> b) -> Parser s b
err     :: Parser s a -> String -> Parser s a
except  :: Parser s a -> Parser s a -> Parser s a
finally :: Parser s a -> ((a, s) -> (b, s)) -> Parser s b
null    :: Monoid a => Parser s a
on_hit  :: Parser s a -> ((a, s) -> Result s b) -> Parser s b
oom     :: Semigroup a => Parser s a -> Parser s a
or      :: Parser s a -> Parser s a -> Parser s a
rep     :: Semigroup a => Int -> Parser s a -> Parser s a
zom     :: Monoid a => Parser s a -> Parser s a
zoo     :: Monoid a => Parser s a -> Parser s a

instance Functor (Result s)
  where
    fmap f (Hit (x, s)) = Hit $ (f x, s)
    fmap _ Miss         = Miss
    fmap _ (Error e)    = Error e

on_miss :: Result s a -> Result s a -> Result s a
on_miss Miss r2 = r2
on_miss r1   _  = r1

or f g = \ x -> f x `on_miss` g x

on_hit' :: Result s a -> ((a, s) -> Result s b) -> Result s b
on_hit' (Hit h)   f = f h
on_hit' Miss      _ = Miss
on_hit' (Error e) _ = Error e

on_hit f g = \ x -> f x `on_hit'` g

and f g = f `on_hit` \ (x, s) -> fmap (x <>) $ g s

rep 1 f = f
rep n f = f `BNF.and` rep (n - 1) f

invert :: Result s a -> (a, s) -> Result s a
invert (Hit _)   _ = Miss
invert Miss      h = Hit h
invert (Error e) _ = Error e

except f g = \ x -> f x `on_hit'` invert (g x)

null s = Hit (mempty, s)

finally f g = f `on_hit` (Hit . g)

conv f g = f `finally` \ (x, s) -> (g x, s)

zoo f = f `BNF.or` BNF.null

zom f = f `BNF.and` zom f `BNF.or` BNF.null

oom f = f `BNF.and` oom f `BNF.or` f

err f e = f `on_hit` return (Error e)
