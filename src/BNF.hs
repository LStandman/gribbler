-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: BNF parser
-- Copyright (C) 2021 LStandman

module BNF(
    Production (..),
    Result (..),
    altr,
    altr1,
    conc,
    conc1,
    exclude,
    one_more,
    override,
    override1,
    rep,
    zero_more,
    zero_one)
  where

import Data.List
import Data.Maybe

infixl 1 `altr`
infixl 1 `altr1`
infixl 1 `conc`
infixl 1 `conc1`
infixl 1 `exclude`
infixl 1 `override`
infixl 1 `override1`

data Result a =
  Match    a      |
  Mismatch        |
  Error    String
  deriving (Show)

type Production a = (a -> Result a)

altr      :: Production a -> Production a -> Production a
altr1     :: Result a -> Result a -> Result a
conc      :: Production a -> Production a -> Production a
conc1     :: Result a -> Production a -> Result a
exclude   :: Production a -> Production a -> Production a
one_more  :: Production a -> Production a
override  :: Production a -> (Production a, Production a) -> Production a
override1 :: Result a -> (Result a, Production a) -> Result a
rep       :: Int -> Production a -> Production a
zero_more :: Production a -> Production a
zero_one  :: Production a -> Production a

override1 Mismatch   t2 = fst t2
override1 (Match v1) t2 = (snd t2) v1
override1 r1         _  = r1

override f (g, h) = \ v -> f v `override1` (g v, h)

altr1 r1 r2 = override1 r1 (r2, \ _ -> r1)

altr f g = \ v -> f v `altr1` g v

conc1 r1 f2 = override1 r1 (Mismatch, f2)

conc f g = \ v -> f v `conc1` g

rep 1 f = f
rep n f = f `conc` (rep (n - 1) f)

exclude f g =
  \ v -> f v `conc1`
  \ u -> g v `override1`
  (Match u, \ _ -> Mismatch)

zero_one  f = f `altr` Match

zero_more f = f `override` (Match, zero_more f)

one_more  f = f `conc` zero_more f
