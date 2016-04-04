{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Language.RM.TypeLevel
import Data.Proxy
import GHC.TypeLits

-- |Initialises R1 to 5, then raises 2 to the power of the value of R1,
-- leaving the result (32) in R0. Uses R2 as a scratch register,
-- thus the machine is initialised with 3 registers
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

              , Halt                        -- 12
              ])) => Proxy r
pow2 = Proxy

result :: Integer
result = natVal pow2

main :: IO ()
main = putStrLn $ "The result of running the machine: " ++ (show result)
