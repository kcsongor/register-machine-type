{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Machine where

import GHC.TypeLits

data Machine where
  Machine :: Nat -> [Reg] -> Machine

-- |Initialise a universal register machine
type family Init (regc :: Nat) :: Machine where
  Init n = 'Machine n (InitList n)

data Reg where
  R :: Nat -> Reg

data Label where
  L :: Nat -> Label

data Instr where
  Incr :: Reg -> Label -> Instr
  Dec  :: Reg -> Label -> Label -> Instr
  Halt :: Instr

type R    = 'R
type L    = 'L
type Dec  = 'Dec
type Incr = 'Incr
type Halt = 'Halt

data Gadget (input :: Reg) (instructions :: [Instr])
  = Gadget

zero :: Gadget r '[Dec r (L 0) (L 1), Halt]
zero = Gadget

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
