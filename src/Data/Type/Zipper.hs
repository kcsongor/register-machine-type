{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Type.Zipper
-- Copyright   :  (C) 2016 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The list zipper data structure with a focused element (at the type-level).
--
-- Used for storing the registers and the instructions because of
-- its performance benefits.
--
-----------------------------------------------------------------------------

module Data.Type.Zipper where

import GHC.TypeLits

-- |Polymorphic list zipper.
data Zipper a where
  Zip :: [a] -> a -> [a] -> Zipper a
  -- |Some functions that operate on Zippers are partial.
  -- Instead of having to mess with promoted Maybes and whatnot, this
  -- constructor represent an erroneous operation.
  Invalid :: Zipper a

-- |Construct `Zipper a` from `[a]`.
-- The list must contain at least one element for the Zipper to be valid.
-- The resulting Zipper focuses on the first element of the list,
-- the tail of the list is found to the right.
type family FromList (xs :: [k]) :: Zipper k where
  FromList '[]
    = 'Invalid
  FromList (x ': xs)
    = 'Zip '[] x xs

-- |Create a list from the Zipper by appending together the left lift, the
-- focused element and the right list in this order.
type family ToList (zipper :: Zipper k) :: [k] where
  ToList ('Zip '[] e n)
    = e ': n
  ToList ('Zip (p ': ps) e n)
    = ToList ('Zip ps p (e ': n))

-- |Shift the focus to the left
type family Left (by :: Nat) (zipper :: Zipper k) :: Zipper k where
  Left 0 z = z
  Left n ('Zip (p ': prevs) cur next)
    = Left (n - 1) ('Zip prevs p (cur ': next))
  Left n z
    = 'Invalid

-- |Shift the focus to the right
type family Right (by :: Nat) (zipper :: Zipper k) :: Zipper k where
  Right 0 z = z
  Right by ('Zip prevs cur (n ': next))
    = Right (by - 1) ('Zip (cur ': prevs) n next)
  Right n z
    = 'Invalid

