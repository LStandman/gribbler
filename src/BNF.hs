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
  Hit a           |
  Miss            |
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
override1 :: Result a -> (Production a, Result a) -> Result a
rep       :: Int -> Production a -> Production a
zero_more :: Production a -> Production a
zero_one  :: Production a -> Production a

override1 (Hit v)   (f,_) = f v
override1 Miss      (_,r) = r
-- Errors short-circuit in every combination.
override1 (Error e) _     = Error e

override f (g, h) = \ v -> f v `override1` (g, h v)

altr1 r1 r2 = r1 `override1` (return r1, r2)

altr f g = \ v -> f v `altr1` g v

conc1 r f = r `override1` (f, Miss)

conc f g = \ v -> f v `conc1` g

rep 1 f = f
rep n f = f `conc` (rep (n - 1) f)

exclude f g =
  \ v -> f v `conc1`
  \ u -> g v `override1` (return Miss, Hit u)

zero_one  f = f `altr` Hit

zero_more f = f `override` (zero_more f, Hit)

one_more  f = f `conc` zero_more f
