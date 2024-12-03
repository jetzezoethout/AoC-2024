module Instruction where

data Instruction
  = Do
  | Dont
  | Mul
      { arg1 :: Int
      , arg2 :: Int
      }

evaluate :: [Instruction] -> Int
evaluate = go
  where
    go []                   = 0
    go (Mul {..}:remaining) = arg1 * arg2 + go remaining
    go (_:remaining)        = go remaining

evaluateConditional :: [Instruction] -> Int
evaluateConditional = go True
  where
    go :: Bool -> [Instruction] -> Int
    go _ [] = 0
    go active (Mul {..}:remaining) =
      if active
        then arg1 * arg2 + go active remaining
        else go active remaining
    go _ (Do:remaining) = go True remaining
    go _ (Dont:remaining) = go False remaining
