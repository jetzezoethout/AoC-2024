module DifferenceSequence where

import           Data.IntMap  (IntMap)
import qualified Data.IntMap  as M
import           Data.IntSet  (IntSet)
import qualified Data.IntSet  as S
import           Data.List    (foldl')
import           SecretNumber (nextSecretNumber)

newtype DifferenceSequence = DifferenceSequence
  { hash :: Int
   -- ^ [a,b,c,d] -> 19^3 (a+9) + 19^2 (b+9) + 19 (c+9) + (d+9)
  } deriving (Show)

empty :: DifferenceSequence
empty = DifferenceSequence 0

cutoffModulus :: Int
cutoffModulus = 19 ^ (3 :: Int)

shiftSequence :: Int -> DifferenceSequence -> DifferenceSequence
shiftSequence newDiff DifferenceSequence {..} =
  DifferenceSequence $ 19 * (hash `mod` cutoffModulus) + (newDiff + 9)

processMonkey :: IntMap Int -> Int -> IntMap Int
processMonkey returnsMap initialSecretNumber =
  go 0 initialSecretNumber empty S.empty returnsMap
  where
    go :: Int -> Int -> DifferenceSequence -> IntSet -> IntMap Int -> IntMap Int
    go 2000 _ _ _ acc = acc
    go stepsTaken secretNumber differenceSequence seen acc =
      let returns = secretNumber `mod` 10
          newSecretNumber = nextSecretNumber secretNumber
          newReturns = newSecretNumber `mod` 10
          newDifference = newReturns - returns
          newDifferenceSequence = shiftSequence newDifference differenceSequence
          newHash = hash newDifferenceSequence
          newAcc =
            if stepsTaken < 4 || newHash `S.member` seen
              then acc
              else M.insertWith (+) newHash newReturns acc
          newSeen =
            if stepsTaken < 4
              then seen
              else newHash `S.insert` seen
       in go
            (stepsTaken + 1)
            newSecretNumber
            newDifferenceSequence
            newSeen
            newAcc

processMonkeys :: [Int] -> IntMap Int
processMonkeys = foldl' processMonkey M.empty

optimalBananas :: [Int] -> Int
optimalBananas initialSecretNumbers =
  maximum $ map snd $ M.toList $ processMonkeys initialSecretNumbers
