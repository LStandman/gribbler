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

data Result a =
    Hit a        |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result b

altr       :: Parser a b -> Parser a b -> Parser a b
conc       :: Semigroup b => Parser [a] b -> Parser [a] b -> Parser [a] b
exclude    :: Parser a b -> Parser a b -> Parser a b
conv       :: Parser a b -> (b -> c) -> Parser a c
match_char :: Char -> Parser String String
nop        :: Monoid b => [a] -> Result b
one_more   :: Semigroup b => Parser [a] b -> Parser [a] b
rep        :: Semigroup b => Int -> Parser [a] b -> Parser [a] b
zero_more  :: Monoid b => Parser [a] b -> Parser [a] b
zero_one   :: Monoid b => Parser [a] b -> Parser [a] b

instance Functor Result
  where
    fmap f (Hit h)   = Hit (f h)
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

altr' :: Result a -> Result a -> Result a
altr' (Hit h)   _ = Hit h
altr' Miss      r = r
altr' (Error e) _ = Error e

altr f g = \ x -> f x `altr'` g x

conc' :: Semigroup a => Result a -> Result a -> Result a
conc' (Hit h1)   (Hit h2)   = Hit (h1 <> h2)
conc' Miss       _          = Miss
conc' (Error e1) _          = Error e1
conc' _          (Error e2) = Error e2
conc' _          _          = Miss

conc f g = \ xs -> case xs of
  _:[] -> Miss
  []   -> Miss
  y:ys -> f [y] `conc'` g ys

rep 1 f = f
rep n f = f `conc` rep (n - 1) f

exclude' :: Result a -> Result a -> Result a
exclude' (Hit h)    Miss       = Hit h
exclude' (Hit _)    (Error e2) = Error e2
exclude' (Error e1) _          = Error e1
exclude' _          _          = Miss

exclude f g = \ x -> f x `exclude'` g x

nop x = Hit mempty

conv f g = \ x -> fmap g $ f x

zero_one  f = f `altr` nop

zero_more f = \ xs -> case (f `conc` zero_more f) xs of
  Hit h   -> Hit h
  Miss    -> Hit mempty
  Error e -> Error e

conc'' :: Semigroup a => Result a -> Result a -> Result a
conc'' (Hit h1)   (Hit h2)   = Hit   (h1 <> h2)
conc'' (Hit h1)   _          = Hit   h1
conc'' (Error e1) _          = Error e1
conc'' _          (Error e2) = Error e2
conc'' _          _          = Miss

one_more f = \ xs -> case xs of
  y:[] -> f [y]
  []   -> Miss
  y:ys -> f [y] `conc''` one_more f ys

match_char c = \ (x:_) -> case c == x of
  True  -> Hit [x]
  False -> Miss
