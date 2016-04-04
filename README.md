# register-machine-type
Universal Register Machine implemented at the type-level of Haskell

_This is just a proof of concept!_

The machine consists of a set of registers, a contiguous instruction pool (starting at index 0)
and supports the following 3 instructions:
- `Inc r l`     - increments register `r` by 1, and then jumps to label `l` (that is the instruction located at index `l`).
- `Dec r l1 l2` - if the value of register `r` is 0, jumps to `l2`, otherwise decrements `r` and jumps to `l1`.
- `Halt`        - halts the machine.

This formulation is identical to the [Lambek machine](https://en.wikipedia.org/wiki/Counter_machine), with the addition of an explicit `Halt` instruction, for convenience. This means that that Haskell's type system is Turing complete (with TypeFamilies and UndecidableInstances).

The machine is initialised with the number of registers required, and a list of instructions to execute, using `Init`. Once initialised, a machine can execute its program with `Execute`.

If the execution of a given program terminates, it will result in the type `Halted ip rs`, where `ip` is the
index of the instruction that halted the machine (there might be multiple `Halt` instructions in the code)
and `rs` is a list representing the resulting state of the registers.

Since the machine is implemented at the type-level of Haskell, the instructions are executed during
the type-checking phase. This means that a program that doesn't terminate will hang the type-checker.

(Be careful with using an on-the-fly type checker editor plugin, as checking a 
non-terminating (or even a relatively complex) program will consume **a lot** of RAM!)

### An example
Initialises R1 to 5, then raises 2 to the power of the value of R1,
leaving the result (32) in R0. Uses R2 as a scratch register,
thus the machine is initialised with 3 registers.

```haskell
pow2 :: ('Halted a (r ': rs) ~
            Execute (Init 3
              '[
              -- Instr              | label index
              -- set R1 to 5
                Inc (R 1) (L 1)             -- 0
              , Inc (R 1) (L 2)             -- 1
              , Inc (R 1) (L 3)             -- 2
              , Inc (R 1) (L 4)             -- 3
              , Inc (R 1) (L 5)             -- 4
              -- set R0 to 1
              , Inc (R 0) (L 6)             -- 5
              -- R0 = 2^R1
              , Dec (R 1) (L 7) (L 12)      -- 6
              -- R2 = R0
              , Dec (R 0) (L 8) (L 9)       -- 7
              , Inc (R 2) (L 7)             -- 8
              -- R0 = 2*R2
              , Dec (R 2) (L 10) (L 6)      -- 9
              , Inc (R 0) (L 11)            -- 10
              , Inc (R 0) (L 9)             -- 11

              , Halt                    -- 12
              ])) => Proxy r
pow2 = Proxy


```

#TODO
- composable gadgets
- more examples
- refactor
