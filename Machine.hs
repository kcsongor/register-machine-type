{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Machine where

import GHC.TypeLits

data Machine where
  M :: Label -> Nat -> [Reg] -> Instructions -> Machine

-- Zipper for (somewhat) more efficient instruction lookup
data Instructions where
  I :: [Instr] -> Instr -> [Instr] -> Instructions

-- |Initialise a universal register machine
-- (TODO) pre: at least one instruction has to be given
type family Init (regc :: Nat) (is :: [Instr]) :: Machine where
  Init n (i ': is) = M (L 0) n (InitList n) (I '[] i is)

data Reg where
  R :: Nat -> Reg

data Label where
  L :: Nat -> Label

data Instr where
  Inc  :: Reg -> Label -> Instr
  Dec  :: Reg -> Label -> Label -> Instr
  Halt :: Instr

type I    = 'I
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
  Execute (M ip rc rs (I prev (Inc r label) next))
    = M label rc rs (I prev (Inc r label) next)
  Execute (M ip rc rs (I prev (Dec r label1 label2) next))
    = M label1 rc rs (I prev (Dec r label1 label2) next)
  Execute m
    = m

-- Utilities for modifying register lists
data Modify = Increment | Decrement

type family ModifyAt (xs :: [Reg]) (at :: Nat) (m :: Modify) :: [Reg] where
  ModifyAt '[] n m
    = '[]
  ModifyAt (R r ': rs) 0 'Increment
    = R (r + 1) ': rs
  ModifyAt (R r ': rs) 0 'Decrement
    = R (r - 1) ': rs
  ModifyAt (r ': rs) n m
    = r ': ModifyAt rs (n - 1) m

type family InitList (size :: Nat) :: [Reg] where
  InitList 0 = '[]
  InitList n = R 0 ': InitList (n - 1)
