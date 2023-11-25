-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: Abstract Backus–Naur form implementation
-- Copyright (C) 2021-2023 LStandman

module BNF(
    Parser (..),
    Result (..),
    conv,
    err,
    et,
    except,
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
infixl 1 `err'`
infixl 1 `et`
infixl 1 `et'`
infixl 1 `look_ahead`
infixl 1 `look_ahead'`
infixl 1 `ou`
infixl 1 `ou'`
infixl 1 `except`

data Result a b =
    Hit (a, b)   |
    Miss         |
    Error String
  deriving (Eq, Show)

type Parser a b = a -> Result a b

conv        :: Parser a b -> (b -> c) -> Parser a c
err         :: Parser a b -> String -> Parser a b
et          :: Semigroup b => Parser a b -> Parser a b -> Parser a b
except      :: Parser a b -> Parser a b -> Parser a b
look_ahead  :: Parser a b -> Parser a b -> Parser a b
nul         :: Monoid b => Parser a b
oom         :: Semigroup b => Parser a b -> Parser a b
ou          :: Parser a b -> Parser a b -> Parser a b
rep         :: Semigroup b => Int -> Parser a b -> Parser a b
zom         :: Monoid b => Parser a b -> Parser a b
zoo         :: Monoid b => Parser a b -> Parser a b

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

except' :: Result a b -> Result a b -> Result a b
except' (Hit ctx)  Miss       = Hit ctx
except' (Hit _)    (Error e2) = Error e2
except' (Error e1) _          = Error e1
except' _          _          = Miss

except f g = \ x -> f x `except'` g x

nul i = Hit (i, mempty)

conv f g = \ x -> fmap g $ f x

zoo f = f `ou` nul

zom f = oom f `ou` nul

maybe' :: Semigroup b => Result a b -> Parser a b -> Result a b
maybe' (Hit (i1, o1)) f = case f i1 of
  Hit ctx2  -> Hit $ fmap (o1 <>) ctx2
  Miss      -> Hit (i1, o1)
  Error e   -> Error e
maybe' Miss        _ = Miss
maybe' (Error e)   _ = Error e

oom f = \ x -> f x `maybe'` oom f

err' :: Result a b -> String -> Result a b
err' (Hit _)    e2 = Error e2
err' Miss       _  = Miss
err' (Error e1) _  = Error e1

err f e = \ x -> f x `err'` e

look_ahead' :: Result a b -> Parser a b -> Result a b
look_ahead' (Hit (i, o)) f = case f i of
  Hit _    -> Hit (i, o)
  Miss     -> Miss
  Error e2 -> Error e2
look_ahead' Miss       _ = Miss
look_ahead' (Error e1) _ = Error e1

look_ahead f g = \ x -> f x `look_ahead'` g
