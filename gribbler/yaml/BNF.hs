-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module BNF(
    Parser,
    Result (..),
    conv,
    err,
    et,
    except,
    finally,
    look_ahead,
    nul,
    oom,
    ou,
    rep,
    zom,
    zoo)
  where

infixl 1 `conv`
infixl 1 `err`
infixl 1 `et`
infixl 1 `finally`
infixl 1 `look_ahead`
infixl 1 `on_hit`
infixl 1 `on_miss`
infixl 1 `ou`
infixl 1 `except`

data Result a b =
    Hit (a, b)   |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result a b

conv       :: Parser a b -> (b -> c) -> Parser a c
err        :: Parser a b -> String -> Parser a b
et         :: Semigroup b => Parser a b -> Parser a b -> Parser a b
except     :: Parser a b -> Parser a b -> Parser a b
finally    :: Parser a b -> ((a, b) -> (a, c)) -> Parser a c
look_ahead :: Parser a b -> Parser a b -> Parser a b
nul        :: Monoid b => Parser a b
oom        :: Semigroup b => Parser a b -> Parser a b
ou         :: Parser a b -> Parser a b -> Parser a b
rep        :: Semigroup b => Int -> Parser a b -> Parser a b
zom        :: Monoid b => Parser a b -> Parser a b
zoo        :: Monoid b => Parser a b -> Parser a b

instance Functor (Result a)
  where
    fmap f (Hit ctx) = Hit $ fmap (f) ctx
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

on_miss :: Result a b -> Result a b -> Result a b
on_miss Miss r2 = r2
on_miss r1   _  = r1

ou f g = \ x -> f x `on_miss` g x

on_hit :: Result a b -> ((a, b) -> Result a c) -> Result a c
on_hit (Hit ctx) f = f ctx
on_hit Miss      _ = Miss
on_hit (Error e) _ = Error e

cat :: Semigroup b => Parser a b -> (a, b) -> Result a b
cat f (i, o) = fmap (o <>) $ f i

et f g = \ x -> f x `on_hit` cat g

rep 1 f = f
rep n f = f `et` rep (n - 1) f

invert :: Result a b -> (a, b) -> Result a b
invert Miss      ctx  = Hit ctx
invert (Hit _)   _    = Miss
invert (Error e) _    = Error e

except f g = \ x -> f x `on_hit` invert (g x)

nul i = Hit (i, mempty)

finally f g = \ x -> f x `on_hit` (Hit . g)

conv f g = f `finally` fmap (g)

zoo f = f `ou` nul

zom f = f `et` zom f `ou` nul

oom f = f `et` oom f `ou` f

err f e = \ x -> f x `on_hit` return (Error e)

peek :: Parser a b -> (a, b) -> Result a b
peek f (i1, o1) = case f i1 of
  Hit _ -> Hit (i1, o1)
  r2    -> r2

look_ahead f g = \ x -> f x `on_hit` peek g
