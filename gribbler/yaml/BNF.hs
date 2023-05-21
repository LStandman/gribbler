-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module BNF(
    Parser (..),
    Result (..),
    conv,
    et,
    non,
    oom,
    ou,
    rep,
    sauf,
    zom,
    zoo)
  where

infixl 1 `conv`
infixl 1 `ou`
infixl 1 `ou'`
infixl 1 `et`
infixl 1 `et'`
infixl 1 `sauf`

data Result a b =
    Hit (a, b)   |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result a b

et           :: Semigroup b => Parser a b -> Parser a b -> Parser a b
non          :: Monoid b => a -> Result a b
oom  :: Semigroup b => Parser a b -> Parser a b
ou           :: Parser a b -> Parser a b -> Parser a b
sauf         :: Parser a b -> Parser a b -> Parser a b
conv         :: Parser a b -> (b -> c) -> Parser a c
rep          :: Semigroup b => Int -> Parser a b -> Parser a b
zom :: Monoid b => Parser a b -> Parser a b
zoo  :: Monoid b => Parser a b -> Parser a b

instance Functor (Result a)
  where
    fmap f (Hit ctx) = Hit $ fmap (f) ctx
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

ou' :: Result a b -> Result a b -> Result a b
ou' (Hit ctx) _ = Hit ctx
ou' Miss      r = r
ou' (Error e) _ = Error e

ou f g = \ x -> f x `ou'` g x

et' :: Semigroup b => Result a b -> Parser a b -> Result a b
et' (Hit (i, o)) f = fmap (o <>) $ f i
et' Miss         _ = Miss
et' (Error e)    _ = Error e

et f g = \ x -> f x `et'` g

rep 1 f = f
rep n f = f `et` rep (n - 1) f

sauf' :: Result a b -> Result a b -> Result a b
sauf' (Hit ctx)  Miss       = Hit ctx
sauf' (Hit _)    (Error e2) = Error e2
sauf' (Error e1) _          = Error e1
sauf' _          _          = Miss

sauf f g = \ x -> f x `sauf'` g x

non i = Hit (i, mempty)

conv f g = \ x -> fmap g $ f x

zoo  f = f `ou` non

zom f = oom f `ou` non

et'' :: Semigroup b => Result a b -> Parser a b -> Result a b
et'' (Hit (i1, o1)) f = case f i1 of
  Hit ctx2  -> Hit $ fmap (o1 <>) ctx2
  Miss      -> Hit (i1, o1)
  Error e   -> Error e
et'' Miss        _ = Miss
et'' (Error e)   _ = Error e

oom f = \ x -> f x `et''` oom f
