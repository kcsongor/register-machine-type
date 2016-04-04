{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Zipper where

import Data.Type.Bool
import GHC.TypeLits

data Zipper a where
  Zip :: [a] -> a -> [a] -> Zipper a

type family ToList (zipper :: Zipper k) :: [k] where
  ToList ('Zip '[] e n)
    = e ': n
  ToList ('Zip (p ': ps) e n)
    = ToList ('Zip ps p (e ': n))

type family Move from to zipper where
  Move from to z
    = If (from <=? to)
           (Right (to - from) z)
           (Left  (from - to) z)

type family Left (by :: Nat) (zipper :: Zipper k) :: Zipper k where
  Left 0 z = z
  Left n ('Zip (p ': prevs) cur next)
    = Left (n - 1) ('Zip prevs p (cur ': next))

type family Right (by :: Nat) (zipper :: Zipper k) :: Zipper k where
  Right 0 z = z
  Right by ('Zip prevs cur (n ': next))
    = Right (by - 1) ('Zip (cur ': prevs) n next)

