-- SPDX-License-Identifier: GPL-3.0-or-later
-- DiffList.hs: DiffList implementation
-- Copyright (C) 2021-2023 LStandman

module DiffList(
    DiffList (..),
    difflist,
    relist)
  where

import Data.Monoid

type DiffList a = Endo ([a])

difflist :: [a] -> DiffList a
difflist l = Endo (l ++)

relist :: DiffList a -> [a]
relist d = appEndo d []
