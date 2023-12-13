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

data Result a b =
    Hit (a, b)   |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result a b

and     :: Semigroup b => Parser a b -> Parser a b -> Parser a b
conv    :: Parser a b -> (b -> c) -> Parser a c
err     :: Parser a b -> String -> Parser a b
except  :: Parser a b -> Parser a b -> Parser a b
finally :: Parser a b -> ((a, b) -> (a, c)) -> Parser a c
null    :: Monoid b => Parser a b
on_hit  :: Parser a b -> ((a, b) -> Result a c) -> Parser a c
oom     :: Semigroup b => Parser a b -> Parser a b
or      :: Parser a b -> Parser a b -> Parser a b
rep     :: Semigroup b => Int -> Parser a b -> Parser a b
zom     :: Monoid b => Parser a b -> Parser a b
zoo     :: Monoid b => Parser a b -> Parser a b

instance Functor (Result a)
  where
    fmap f (Hit ctx) = Hit $ fmap (f) ctx
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

on_miss :: Result a b -> Result a b -> Result a b
on_miss Miss r2 = r2
on_miss r1   _  = r1

or f g = \ x -> f x `on_miss` g x

on_hit' :: Result a b -> ((a, b) -> Result a c) -> Result a c
on_hit' (Hit ctx) f = f ctx
on_hit' Miss      _ = Miss
on_hit' (Error e) _ = Error e

on_hit f g = \ x -> f x `on_hit'` g

cat :: Semigroup b => Parser a b -> (a, b) -> Result a b
cat f (i, o) = fmap (o <>) $ f i

and f g = f `on_hit` cat g

rep 1 f = f
rep n f = f `BNF.and` rep (n - 1) f

invert :: Result a b -> (a, b) -> Result a b
invert (Hit _)   _    = Miss
invert Miss      ctx  = Hit ctx
invert (Error e) _    = Error e

except f g = \ x -> f x `on_hit'` invert (g x)

null i = Hit (i, mempty)

finally f g = f `on_hit` (Hit . g)

conv f g = f `finally` fmap (g)

zoo f = f `BNF.or` BNF.null

zom f = f `BNF.and` zom f `BNF.or` BNF.null

oom f = f `BNF.and` oom f `BNF.or` f

err f e = f `on_hit` return (Error e)
