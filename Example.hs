{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Machine
import Data.Proxy
import GHC.TypeLits

-- |Initialises R1 to 5, then raises 2 to the power of the value of R1,
-- leaving the result (32) in R0. Uses R2 as a scratch register,
-- thus the machine is initialised with 3 registers
pow2 :: ('Halted a ((R r) ': rs) ~
            Execute (Init 3
              '[
              -- set R1 to 5
                Inc 1 (L 1)
              , Inc 1 (L 2)
              , Inc 1 (L 3)
              , Inc 1 (L 4)
              , Inc 1 (L 5)
              -- set R0 to 1
              , Inc 0 (L 6)
              -- R0 = 2^R1
              , Dec 1 (L 7) (L 12)
              -- R2 = R0
              , Dec 0 (L 8) (L 9)
              , Inc 2 (L 7)
              -- R0 = 2*R2
              , Dec 2 (L 10) (L 6)
              , Inc 0 (L 11)
              , Inc 0 (L 9)

              , Halt
              ])) => Proxy r
pow2 = Proxy

result :: Integer
result = natVal pow2

main :: IO ()
main = putStrLn $ "The result of running the machine: " ++ (show result)
