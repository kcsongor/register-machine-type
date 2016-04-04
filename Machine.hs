{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Machine where

import GHC.TypeLits
import Data.Type.Bool

data Machine where
  M :: Label -> Nat -> Registers -> Instructions -> Machine

-- Zipper for (somewhat) more efficient instruction lookup
data Instructions where
  Is :: [Instr] -> Instr -> [Instr] -> Instructions

data Registers where
  Rs :: [Reg] -> Reg -> [Reg] -> Registers

-- |Initialise a universal register machine
-- (TODO) pre: at least one instruction has to be given
type family Init (regc :: Nat) (is :: [Instr]) :: Machine where
  Init n (i ': is)
    = M (L 0) n (Rs '[] (R 0) (InitList (n - 1))) (Is '[] i is)

data Reg where
  R :: Nat -> Reg

data Label where
  L :: Nat -> Label

data Instr where
  Inc  :: Reg -> Label -> Instr
  Dec  :: Reg -> Label -> Label -> Instr
  Halt :: Instr

type Is   = 'Is
type Rs   = 'Rs
type M    = 'M
type R    = 'R
type L    = 'L
type Dec  = 'Dec
type Inc  = 'Inc
type Halt = 'Halt

data Gadget (input :: Reg) (instructions :: [Instr])
  = Gadget

zero :: Gadget r '[Dec r (L 0) (L 1), Halt]
zero = Gadget

type family Execute (m :: Machine) :: Machine where
  Execute (M ip rc rs (Is prev (Inc r label) next))
    = M label rc rs (Is prev (Inc r label) next)
  Execute (M ip rc rs (Is prev (Dec r label1 label2) next))
    = M label1 rc rs (Is prev (Dec r label1 label2) next)
  Execute m
    = m

-- Zipper utilities
type family Jump from to zipper where
  Jump from to z
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

type family InitList (size :: Nat) :: [Reg] where
  InitList 0 = '[]
  InitList n = R 0 ': InitList (n - 1)
