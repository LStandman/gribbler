-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module BNF(
    Parser (..),
    Result (..),
    et,
    non,
    one_or_more,
    ou,
    rep,
    sauf,
    zero_or_more,
    zero_or_one)
  where

infixl 1 `ou`
infixl 1 `ou'`
infixl 1 `et`
infixl 1 `et'`
infixl 1 `sauf`

data Result a =
    Hit a        |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser a = a -> Result a

et        :: Parser a -> Parser a -> Parser a
non       :: a -> Result a
one_or_more  :: Parser a -> Parser a
ou        :: Parser a -> Parser a -> Parser a
rep       :: Int -> Parser a -> Parser a
sauf      :: Parser a -> Parser a -> Parser a
zero_or_more :: Parser a -> Parser a
zero_or_one  :: Parser a -> Parser a

instance Functor Result
  where
    fmap f (Hit h)   = Hit (f h)
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

ou' :: Result a -> Result a -> Result a
ou' (Hit h)   _ = Hit h
ou' Miss      r = r
ou' (Error e) _ = Error e

ou f g = \ x -> f x `ou'` g x

et' :: Result a -> Parser a -> Result a
et' (Hit h)   f = f h
et' Miss      _ = Miss
et' (Error e) _ = Error e

et f g = \ x -> f x `et'` g

rep 1 f = f
rep n f = f `et` rep (n - 1) f

sauf' :: Result a -> Result a -> Result a
sauf' (Hit h)    Miss     = Hit h
sauf' (Hit _)  (Error e2) = Error e2
sauf' (Error e1) _        = Error e1
sauf' _          _        = Miss

sauf f g = \ x -> f x `sauf'` g x

non h = Hit h

zero_or_one  f = f `ou` non

zero_or_more f = one_or_more f `ou` non

one_or_more f = f `et` (one_or_more f `ou` non)
