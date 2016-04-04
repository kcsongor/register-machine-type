{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.RM.TypeLevel (
    type R
  , type L
  , type Execute
  , type Init
  , type Inc
  , type Dec
  , type Halt
  , Machine (..)
) where

import GHC.TypeLits
import Data.Type.Bool

data Machine where
  M :: Label -> Ptr -> Nat -> Registers -> Instructions -> Machine
  Halted :: Label -> [Nat] -> Machine

-- Zipper for (somewhat) more efficient instruction lookup
data Instructions where
  Is :: [Instr] -> Instr -> [Instr] -> Instructions

data Registers where
  Rs :: [Nat] -> Nat -> [Nat] -> Registers

-- |Initialise a universal register machine
-- (TODO) pre: at least one instruction has to be given
type family Init (regc :: Nat) (is :: [Instr]) :: Machine where
  Init n (i ': is)
    = M (L 0) (R 0) n (Rs '[] 0 (InitList (n - 1))) (Is '[] i is)

data Label where
  L :: Nat -> Label

data Ptr where
  P :: Nat -> Ptr

data Instr where
  Inc  :: Ptr -> Label -> Instr
  Dec  :: Ptr -> Label -> Label -> Instr
  Halt :: Instr

type Is   = 'Is
type Rs   = 'Rs
type M    = 'M
type R    = 'P
type L    = 'L
type Dec  = 'Dec
type Inc  = 'Inc
type Halt = 'Halt

-- TODO: compose (and use :P) gadgets
data Gadget (input :: Ptr) (instructions :: [Instr])
  = Gadget

zero :: Gadget r '[Dec r (L 0) (L 1), Halt]
zero = Gadget

type family Execute (m :: Machine) :: Machine where
  -- |When the register pointer `ptr' is the same as the register specified
  -- in the in the instruction, we set the instrction pointer `ip' to the
  -- destination label, and move the instruction zipper to that position.
  -- Also, increment the currently focused register
  Execute (M ip ptr rc (Rs p c n) (Is prev (Inc ptr label) next))
    = Execute (
          M label ptr rc (Rs p (c + 1) n)
            (Jump ip label (Is prev (Inc ptr label) next)))

  -- |When the register pointer `ptr' is not the same, we need to jump to
  -- the required register before doing anything
  Execute (M ip ptr rc rs (Is prev (Inc r label) next))
    = Execute (M ip r rc (Jump ptr r rs) (Is prev (Inc r label) next))

  Execute (M ip ptr rc (Rs p c n) (Is prev (Dec ptr label1 label2) next))
    = Execute ( If (c <=? 0)
                  (M label2 ptr rc (Rs p c n)
                    (Jump ip label2 (Is prev (Dec ptr label1 label2) next)))
                  (M label1 ptr rc (Rs p (c - 1) n)
                    (Jump ip label1 (Is prev (Dec ptr label1 label2) next)))
            )

  -- |When the register pointer `ptr' is not the same, we need to jump to
  -- the required register before doing anything
  Execute (M ip ptr rc rs (Is prev (Dec r label1 label2) next))
    = Execute (M ip r rc (Jump ptr r rs) (Is prev (Dec r label1 label2) next))

  Execute (M ip ptr rc rs is)
    = 'Halted ip (ToList rs)

type family AddressOf (a :: k) :: Nat where
  AddressOf (R p) = p
  AddressOf (L l) = l

-- Zipper utilities
type family ToList zipper where
  ToList (Rs '[] e n)
    = e ': n
  ToList (Rs (p ': ps) e n)
    = ToList (Rs ps p (e ': n))

type family Jump from to zipper where
  Jump from to z = Jump' (AddressOf from) (AddressOf to) z

type family Jump' from to zipper where
  Jump' from to z
    = If (from <=? to)
           (Right (to - from) z)
           (Left  (from - to) z)

type family Left (by :: Nat) (zipper :: k) :: k where
  Left 0 z = z
  Left n (Is (p ': prevs) cur next)
    = Left (n - 1) (Is prevs p (cur ': next))
  Left n (Rs (p ': prevs) cur next)
    = Left (n - 1) (Rs prevs p (cur ': next))

type family Right (by :: Nat) (zipper :: k) :: k where
  Right 0 z = z
  Right by (Is prevs cur (n ': next))
    = Right (by - 1) (Is (cur ': prevs) n next)
  Right by (Rs prevs cur (n ': next))
    = Right (by - 1) (Rs (cur ': prevs) n next)

type family InitList (size :: Nat) :: [Nat] where
  InitList 0 = '[]
  InitList n = 0 ': InitList (n - 1)
