module SecretNumber where

import           Data.Bits ((.^.))

mix :: Int -> Int -> Int
mix secretNumber val = secretNumber .^. val

prune :: Int -> Int
prune secretNumber = secretNumber `mod` 16777216

step1 :: Int -> Int
step1 secretNumber = prune $ mix secretNumber $ secretNumber * 64

-- No pruning necessary because the numbers become smaller
step2 :: Int -> Int
step2 secretNumber = mix secretNumber $ secretNumber `div` 32

step3 :: Int -> Int
step3 secretNumber = prune $ mix secretNumber $ secretNumber * 2048

nextSecretNumber :: Int -> Int
nextSecretNumber = step3 . step2 . step1

generate :: Int -> [Int]
generate = go
  where
    go secretNumber = secretNumber : go (nextSecretNumber secretNumber)
