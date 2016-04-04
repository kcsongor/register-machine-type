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
  , type Run
  , type Inc
  , type Dec
  , type Halt
  , Machine (..)
) where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Zipper

data Machine where
  M :: Label -> Ptr -> Zipper Nat -> Zipper Instr -> Machine
  Halted :: Label -> [Nat] -> Machine

type Run is = Execute (Init is)

-- |Initialise a universal register machine
-- (TODO) pre: at least one instruction has to be given
type family Init (is :: [Instr]) :: Machine where
  Init is
    = M (L 0) (R 0) (FromList (Replicate (RequiredRegisters is) 0)) (FromList is)

data Label where
  L :: Nat -> Label

data Ptr where
  P :: Nat -> Ptr

data Instr where
  Inc  :: Ptr -> Label -> Instr
  Dec  :: Ptr -> Label -> Label -> Instr
  Halt :: Instr

type Is   = 'Zip
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
  -- When the register pointer `ptr' is the same as the register specified
  -- in the in the instruction, we set the instrction pointer `ip' to the
  -- destination label, and move the instruction zipper to that position.
  -- Also, increment the currently focused register
  Execute (M ip ptr rs (Is prev (Inc ptr label) next))
    = Execute (
          M label ptr (Replace rs (Extract rs + 1))
            (Jump ip label (Is prev (Inc ptr label) next)))

  -- When the register pointer `ptr' is not the same, we need to jump to
  -- the required register before doing anything
  Execute (M ip ptr rs (Is prev (Inc r label) next))
    = Execute (M ip r (Jump ptr r rs) (Is prev (Inc r label) next))

  Execute (M ip ptr rs (Is prev (Dec ptr label1 label2) next))
    = Execute ( If (Extract rs <=? 0)
                  (M label2 ptr rs
                    (Jump ip label2 (Is prev (Dec ptr label1 label2) next)))
                  (M label1 ptr (Replace rs (Extract rs - 1))
                    (Jump ip label1 (Is prev (Dec ptr label1 label2) next)))
            )

  -- When the register pointer `ptr' is not the same, we need to jump to
  -- the required register before doing anything
  Execute (M ip ptr rs (Is prev (Dec r label1 label2) next))
    = Execute (M ip r (Jump ptr r rs) (Is prev (Dec r label1 label2) next))

  Execute (M ip ptr rs is)
    = 'Halted ip (ToList rs)

-- |Retrieve the number of registers required for running the instructions.
-- This is needed for initialising the machine
type family RequiredRegisters (is :: [Instr]) :: Nat where
  RequiredRegisters '[] = 0
  RequiredRegisters (Inc (R r) n ': xs)
    = Max (r + 1) (RequiredRegisters xs)
  RequiredRegisters (Dec (R r) t f ': xs)
    = Max (r + 1) (RequiredRegisters xs)
  RequiredRegisters (x ': xs)
    = RequiredRegisters xs

-- |The ordinary max function lifted to the type-level
type family Max a b where
  Max a b = If (a <=? b) b a

-- Misc utilities for the data structures
type family AddressOf (a :: k) :: Nat where
  AddressOf (R p) = p
  AddressOf (L l) = l

-- |Move the focused element of the zipper from a given global position to
-- another
type family Jump from to zipper where
  Jump from to z = Jump' (AddressOf from) (AddressOf to) z

type family Jump' from to zipper where
  Jump' from to z
    = If (from <=? to)
           (Right (to - from) z)
           (Left  (from - to) z)

-- |The ordinary replicate function lifted to the type-level
type family Replicate (times :: Nat) (x :: k) :: [k] where
  Replicate 0 x = '[]
  Replicate n x = x ': Replicate (n - 1) x
