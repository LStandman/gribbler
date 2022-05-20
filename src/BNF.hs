-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: BNF parser
-- Copyright (C) 2021-2022 LStandman

module BNF(
    Production (..),
    Resultant (..),
    altr,
    conc,
    exclude,
    finally,
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
infixl 1 `finally`

data Resultant a =
    Hit {
      output :: a,
      stream :: String} |
    Miss                |
    Error String
  deriving (Show)

type Production a = (String -> Resultant a)

altr      :: Production a -> Production a -> Production a
conc      :: Semigroup a => Production a -> Production a -> Production a
exclude   :: Production a -> Production a -> Production a
finally   :: Production a -> (a -> b) -> Production b
one_more  :: Monoid a => Production a -> Production a
rep       :: Semigroup a => Int -> Production a -> Production a
zero_more :: Monoid a => Production a -> Production a
zero_one  :: Monoid a => Production a -> Production a

instance Functor Resultant
  where
    fmap f (Hit o s) = Hit (f o) s
    fmap _ Miss      = Miss
    fmap _ (Error e) = Error e

altr' :: Resultant a -> Resultant a -> Resultant a
altr' (Hit o s) _ = Hit o s
altr' Miss      r = r
altr' (Error e) _ = Error e

altr f g = \ s -> f s `altr'` g s

conc' :: Semigroup a => Resultant a -> Production a -> Resultant a
conc' (Hit o1 s1) f = fmap (o1 <>) $ f s1
conc' Miss        _ = Miss
conc' (Error e)   _ = Error e

conc f g = \ s -> f s `conc'` g

rep 1 f = f
rep n f = f `conc` rep (n - 1) f

exclude' :: Resultant a -> Resultant a -> Resultant a
exclude' (Hit o s)   Miss       = Hit o s
exclude' (Error e1)  _          = Error e1
exclude' _           (Error e2) = Error e2
exclude' _           _          = Miss

exclude f g = \ s -> f s `exclude'` g s

nop :: Monoid a => String -> Resultant a
nop s = Hit mempty s

finally f g = \ s -> fmap g $ f s

zero_one  f = f `altr` nop

zero_more f s1 = case f $ s1 of
  Hit o2 s2 -> Hit o2 s2 `conc'` zero_more f
  Miss      -> nop s1
  Error e   -> Error e

one_more  f = f `conc` zero_more f
