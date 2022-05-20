-- SPDX-License-Identifier: GPL-3.0-or-later
-- BNF.hs: BNF parser
-- Copyright (C) 2021 LStandman

module BNF(
    Production (..),
    Resultant (..),
    altr,
    conc,
    conc1,
    exclude,
    finally,
    finally1,
    one_more,
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
infixl 1 `finally`
infixl 1 `finally1`

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
conc1     :: Semigroup a => Resultant a -> Production a -> Resultant a
exclude   :: Production a -> Production a -> Production a
finally   :: Production a -> (a -> b) -> Production b
finally1  :: Resultant a -> (a -> b) -> Resultant b
one_more  :: Monoid a => Production a -> Production a
rep       :: Semigroup a => Int -> Production a -> Production a
zero_more :: Monoid a => Production a -> Production a
zero_one  :: Monoid a => Production a -> Production a

altr1 :: Resultant a -> Resultant a -> Resultant a
altr1 (Hit o s) _ = Hit o s
altr1 Miss      r = r
altr1 (Error e) _ = Error e

altr f g = \ s -> f s `altr1` g s

conc1 (Hit o1 s1) f = g . f $ s1
  where
    g (Hit o2 s2) = Hit (o1 <> o2) s2
    g r           = r
conc1 Miss        _ = Miss
conc1 (Error e)   _ = Error e

conc f g = \ s -> f s `conc1` g

rep 1 f = f
rep n f = f `conc` (rep (n - 1) f)

exclude1 :: Resultant a -> Resultant a -> Resultant a
exclude1 (Hit o s)   Miss       = Hit o s
exclude1 (Error e1)  _          = Error e1
exclude1 _           (Error e2) = Error e2
exclude1 _           _          = Miss

exclude f g = \ s -> f s `exclude1` g s

nop :: Monoid a => String -> Resultant a
nop s = Hit mempty s

finally1 (Hit o s) f = Hit (f o) s
finally1 Miss      _ = Miss
finally1 (Error e) _ = Error e

finally f g = \ s -> f s `finally1` g

zero_one  f = f `altr` nop

zero_more f s1 = g . f $ s1
  where
    g (Hit o2 s2) = (Hit o2 s2) `conc1` (zero_more f)
    g Miss        = nop s1
    g (Error e)   = Error e

one_more  f = f `conc` zero_more f
