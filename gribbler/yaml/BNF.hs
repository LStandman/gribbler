-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Backus–Naur form implementation
-- Copyright (C) 2021-2022 LStandman

module BNF(
    Parser (..),
    Result (..),
    altr,
    conc,
    conv,
    exclude,
    match_char,
    nop,
    one_more,
    rep,
    zero_more,
    zero_one)
  where

import Data.List
import Data.Maybe

infixl 1 `altr`
infixl 1 `altr'`
infixl 1 `conc`
infixl 1 `conc'`
infixl 1 `exclude`
infixl 1 `conv`

data Result a b =
    Hit {
      stream :: a,
      parse  :: b} |
    Miss                |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result a b

altr       :: Parser a b -> Parser a b -> Parser a b
conc       :: Semigroup b => Parser a b -> Parser a b -> Parser a b
exclude    :: Parser a b -> Parser a b -> Parser a b
conv       :: Parser a b -> (b -> c) -> Parser a c
match_char :: Char -> Parser String String
nop        :: Monoid b => a -> Result a b
one_more   :: Semigroup b => Parser a b -> Parser a b
rep        :: Semigroup b => Int -> Parser a b -> Parser a b
zero_more  :: Monoid b => Parser a b -> Parser a b
zero_one   :: Monoid b => Parser a b -> Parser a b

instance Functor (Result a)
  where
    fmap f (Hit s p) = Hit s (f p)
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

altr' :: Result a b -> Result a b -> Result a b
altr' (Hit s p) _ = Hit s p
altr' Miss      r = r
altr' (Error e) _ = Error e

altr f g = \ x -> f x `altr'` g x

conc' :: Semigroup b => Result a b -> Parser a b -> Result a b
conc' (Hit s p)   f = fmap (p <>) $ f s
conc' Miss        _ = Miss
conc' (Error e)   _ = Error e

conc f g = \ x -> f x `conc'` g

rep 1 f = f
rep n f = f `conc` rep (n - 1) f

exclude' :: Result a b -> Result a b -> Result a b
exclude' (Hit s p)  Miss       = Hit s p
exclude' (Hit _ _)  (Error e2) = Error e2
exclude' (Error e1) _          = Error e1
exclude' _          _          = Miss

exclude f g = \ x -> f x `exclude'` g x

nop x = Hit x mempty

conv f g = \ x -> fmap g $ f x

zero_one  f = f `altr` nop

zero_more f = \ x -> case (f `conc` zero_more f) x of
  Hit s p -> Hit s p
  Miss    -> Hit x mempty
  Error e -> Error e

conc'' :: Semigroup b => Result a b -> Parser a b -> Result a b
conc'' (Hit s1 p1) f = case f s1 of
  Hit s2 p2 -> Hit s2 (p1 <> p2)
  Miss      -> Hit s1 p1
  Error e   -> Error e
conc'' Miss        _ = Miss
conc'' (Error e)   _ = Error e

one_more f = \ x -> f x `conc''` one_more f

match_char c = \ xs -> case xs of
  []     -> Miss
  (y:ys) -> case c == y of
    True  -> Hit ys [y]
    False -> Miss
