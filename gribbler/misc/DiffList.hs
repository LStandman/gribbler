-- SPDX-License-Identifier: GPL-3.0-or-later
-- DiffList.hs: DiffList implementation
-- Copyright (C) 2021-2023 LStandman

module DiffList(
    DiffList (..),
    DiffString,
    difflist,
    relist)
  where

import Data.Monoid

newtype DiffList a = DiffList (Endo ([a]))
type DiffString = DiffList Char

difflist :: [a] -> DiffList a
relist :: DiffList a -> [a]

instance Semigroup (DiffList a)
  where
    DiffList x <> DiffList y = DiffList $ x <> y

instance Monoid (DiffList a)
  where
    mempty = difflist []

instance Eq a => Eq (DiffList a)
  where
    x == y = relist x == relist y

instance Show a => Show (DiffList a)
  where
    show = show . relist

difflist l = DiffList $ Endo (l ++)

relist (DiffList d) = appEndo d []
