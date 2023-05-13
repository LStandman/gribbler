-- SPDX-License-Identifier: GPL-3.0-or-later
-- Common.hs: Miscellaneous definitions
-- Copyright (C) 2021-2022 LStandman

module Misc(
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
