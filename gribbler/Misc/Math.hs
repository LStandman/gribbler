-- SPDX-License-Identifier: GPL-3.0-or-later
-- Misc/Math.hs: Math utilities
-- Copyright (C) 2024 LStandman

module Misc.Math
  ( div1,
  )
where

infixl 7 `div1`

div1 :: Integral a => a -> a -> a
a `div1` b = (a + b - 1) `div` b
