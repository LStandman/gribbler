-- SPDX-License-Identifier: GPL-3.0-or-later
-- DiffList.hs: DiffList implementation
-- Copyright (C) 2021-2023 LStandman

module DiffList(
    DiffList (..),
    difflist,
    relist)
  where

import Data.Monoid

data DiffList a = DiffList (Endo ([a]))

instance Semigroup (DiffList a)
  where
    DiffList x <> DiffList y = DiffList $ x <> y

difflist :: [a] -> DiffList a
difflist l = DiffList $ Endo (l ++)

relist :: DiffList a -> [a]
relist (DiffList d) = appEndo d []

instance Monoid (DiffList a)
  where
    mempty = difflist []

instance Eq a => Eq (DiffList a)
  where
    x == y = relist x == relist y

instance Show a => Show (DiffList a)
  where
    show = show . relist
