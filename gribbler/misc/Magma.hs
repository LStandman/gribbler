-- SPDX-License-Identifier: GPL-3.0-or-later
-- Magma.hs: Magma definition
-- Copyright (C) 2023 LStandman

module Magma(
    Magma(..),
    UnitalMagma(..))
  where

import Data.Monoid

infixl 1 <>

class Magma a
  where
    (<>) :: a -> a -> a

class Magma a => UnitalMagma a
  where
    mempty :: a

instance Magma [a]
  where
    (<>) = (Data.Monoid.<>)

instance UnitalMagma [a]
  where
    mempty = Data.Monoid.mempty
