-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/LookupTable.hs: Lookup table implementation
-- Copyright (C) 2023 LStandman

module Misc.LookupTable(
    LookupTable (..),
    Misc.LookupTable.lookup)
  where

newtype LookupTable a b =
    LookupTable [(a, b)]
  deriving (Eq, Show)

lookup :: Eq a => a -> LookupTable a b -> Maybe b

lookup k (LookupTable t) = Prelude.lookup k t
