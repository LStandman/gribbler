-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Backus–Naur form implementation
-- Copyright (C) 2021-2022 LStandman

module BNF(
    Parser (..),
    Result (..),
    ou,
    et,
    conv,
    sauf,
    non,
    one_more,
    rep,
    zero_more,
    zero_one)
  where

import Data.List
import Data.Maybe

infixl 1 `ou`
infixl 1 `ou'`
infixl 1 `et`
infixl 1 `et'`
infixl 1 `sauf`
infixl 1 `conv`

data Result a b =
    Hit {
      stream :: a,
      parse  :: b} |
    Miss                |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result a b

ou        :: Parser a b -> Parser a b -> Parser a b
et        :: Semigroup b => Parser a b -> Parser a b -> Parser a b
sauf      :: Parser a b -> Parser a b -> Parser a b
conv      :: Parser a b -> (b -> c) -> Parser a c
non       :: Monoid b => a -> Result a b
one_more  :: Semigroup b => Parser a b -> Parser a b
rep       :: Semigroup b => Int -> Parser a b -> Parser a b
zero_more :: Monoid b => Parser a b -> Parser a b
zero_one  :: Monoid b => Parser a b -> Parser a b

instance Functor (Result a)
  where
    fmap f (Hit s p) = Hit s (f p)
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

ou' :: Result a b -> Result a b -> Result a b
ou' (Hit s p) _ = Hit s p
ou' Miss      r = r
ou' (Error e) _ = Error e

ou f g = \ x -> f x `ou'` g x

et' :: Semigroup b => Result a b -> Parser a b -> Result a b
et' (Hit s p)   f = fmap (p <>) $ f s
et' Miss        _ = Miss
et' (Error e)   _ = Error e

et f g = \ x -> f x `et'` g

rep 1 f = f
rep n f = f `et` rep (n - 1) f

sauf' :: Result a b -> Result a b -> Result a b
sauf' (Hit s p)  Miss       = Hit s p
sauf' (Hit _ _)  (Error e2) = Error e2
sauf' (Error e1) _          = Error e1
sauf' _          _          = Miss

sauf f g = \ x -> f x `sauf'` g x

non x = Hit x mempty

conv f g = \ x -> fmap g $ f x

zero_one  f = f `ou` non

zero_more f = one_more f `ou` non

et'' :: Semigroup b => Result a b -> Parser a b -> Result a b
et'' (Hit s1 p1) f = case f s1 of
  Hit s2 p2 -> Hit s2 (p1 <> p2)
  Miss      -> Hit s1 p1
  Error e   -> Error e
et'' Miss        _ = Miss
et'' (Error e)   _ = Error e

one_more f = \ x -> f x `et''` one_more f
