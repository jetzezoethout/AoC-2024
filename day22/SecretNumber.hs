module SecretNumber where

import           Data.Bits (xor)

mix :: Int -> Int -> Int
mix secretNumber val = secretNumber `xor` val

prune :: Int -> Int
prune secretNumber = secretNumber `mod` 16777216

step1 :: Int -> Int
step1 secretNumber = prune $ mix secretNumber $ secretNumber * 64

step2 :: Int -> Int
step2 secretNumber = prune $ mix secretNumber $ secretNumber `div` 32

step3 :: Int -> Int
step3 secretNumber = prune $ mix secretNumber $ secretNumber * 2048

newSecretNumber :: Int -> Int
newSecretNumber = step3 . step2 . step1

generate :: Int -> [Int]
generate = go
  where
    go secretNumber = secretNumber : go (newSecretNumber secretNumber)
