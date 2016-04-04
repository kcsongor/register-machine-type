{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.RM.TypeLevel
-- Copyright   :  (C) 2016 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Computationally universal register machine implementation at the type-level.
--
-- This formulation is identical to the [Lambek
-- machine](https://en.wikipedia.org/wiki/Counter_machine), with the addition
-- of an explicit `Halt` instruction, for convenience.
--
-- This means (or rather, this is made possible by the fact) that that
-- Haskell's type system is Turing complete (at least with
-- TypeFamilies and UndecidableInstances).
--
-----------------------------------------------------------------------------

module Language.RM.TypeLevel (
  -- * Construct and run the machine
    type Run
  , Machine (Halted)
  -- * Instructions
  , Instr (..)
  , type Inc
  , type Dec
  , type Halt
  -- * Operands
  , Ptr (..)
  , type R
  , Label (..)
  , type L
) where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Zipper

-- |GADT representing the register machine
data Machine where
  M :: Label -> Ptr -> Zipper Nat -> Zipper Instr -> Machine
  Halted :: Label -> [Nat] -> Machine

-- |Run a list of instructions (`Instr`)
--
-- If the program terminates, the result is a `Halted` machine.
-- (See the [Halting problem](https://en.wikipedia.org/wiki/Halting_problem))
--
-- Otherwise the execution hangs the type-checker and eventually consumes
-- all the available RAM.
type family Run (is :: [Instr]) :: Machine where
  Run is = Execute (Init is)

-- |Initialise a register machine
-- (TODO) pre: at least one instruction has to be given
type family Init (is :: [Instr]) :: Machine where
  Init is
    = M (L 0) (R 0) (FromList (Replicate (RequiredRegisters is) 0)) (FromList is)

data Label where
  L :: Nat -> Label

-- |Pointer to a register.
data Ptr where
  Ptr :: Nat -> Ptr

-- |The GADT representing the intructions
--
-- The following 3 instructions are supported:
--
-- @Inc r l@     - increments register @r@ by 1, and then jumps to label @l@
-- (that is the instruction located at index @l@).
--
-- @Dec r l1 l2@ - if the value of register @r@ is 0, jumps to @l2@, otherwise
-- decrements @r@ and jumps to @l1@.
--
-- @Halt@        - halts the machine.
--
-- The `Ptr` arguments are specifying the register.
--
-- Some examples
--
-- @
-- Inc (R 1) (L 1)
-- Dec (R 1) (L 7) (L 12)
-- Halt
-- @
--
data Instr where
  Inc  :: Ptr -> Label -> Instr
  Dec  :: Ptr -> Label -> Label -> Instr
  Halt :: Instr

-- |Alias to avoid having to tick the promoted `Ptr` constructor
type R    = 'Ptr

-- |Alias to avoid having to tick the promoted `L` constructor
type L    = 'L

-- |Alias to avoid having to tick the promoted `Dec` constructor
type Dec  = 'Dec

-- |Alias to avoid having to tick the promoted `Inc` constructor
type Inc  = 'Inc

-- |Alias to avoid having to tick the promoted `Halt` constructor
type Halt = 'Halt

-- Other aliases
type Is   = 'Zip
type M    = 'M


-- TODO: compose (and use :P) gadgets
data Gadget (input :: Ptr) (instructions :: [Instr])
  = Gadget

zero :: Gadget r '[Dec r (L 0) (L 1), Halt]
zero = Gadget

-- Execute an initialised machine
type family Execute (m :: Machine) :: Machine where
  -- When the register pointer `ptr' is the same as the register specified
  -- in the in the instruction, we set the instruction pointer `ip' to the
  -- destination label, and move the instruction zipper to that position.
  -- Also, increment the currently focused register
  Execute (M ip ptr rs (Is prev (Inc ptr label) next))
    = Execute (
          M label ptr (Replace rs (Extract rs + 1))
            (Jump ip label (Is prev (Inc ptr label) next)))

  Execute (M ip ptr rs (Is prev (Dec ptr label1 label2) next))
    = Execute ( If (Extract rs <=? 0)
                  (M label2 ptr rs
                    (Jump ip label2 (Is prev (Dec ptr label1 label2) next)))
                  (M label1 ptr (Replace rs (Extract rs - 1))
                    (Jump ip label1 (Is prev (Dec ptr label1 label2) next)))
            )

  -- When the register pointer `ptr' is not the same as the focus,
  -- we need to focus the required register before doing anything
  Execute (M ip ptr rs (Is prev (Inc r label) next))
    = Execute (M ip r (Jump ptr r rs) (Is prev (Inc r label) next))
  Execute (M ip ptr rs (Is prev (Dec r label1 label2) next))
    = Execute (M ip r (Jump ptr r rs) (Is prev (Dec r label1 label2) next))

  -- Base case: halt the machine
  -- This can occur if we've reached a Halt instruction, or if the
  -- label we tried to access is not defined.
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
