module DifferenceSequence where

import           Control.Monad               (unless)
import           Control.Monad.ST
import           Data.Foldable               (traverse_)
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V
import           SecretNumber                (nextSecretNumber)

newtype DifferenceSequence = DifferenceSequence
  { hash :: Int
   -- ^ [a,b,c,d] -> 19^3 (a+9) + 19^2 (b+9) + 19 (c+9) + (d+9)
  } deriving (Show)

emptyDiffSequence :: DifferenceSequence
emptyDiffSequence = DifferenceSequence 0

cutoffModulus :: Int
cutoffModulus = 19 ^ (3 :: Int)

shiftSequence :: Int -> DifferenceSequence -> DifferenceSequence
shiftSequence newDiff DifferenceSequence {..} =
  DifferenceSequence $ 19 * (hash `mod` cutoffModulus) + (newDiff + 9)

totalHashes :: Int
totalHashes = 19 ^ (4 :: Int)

processMonkey :: MVector s Int -> Int -> ST s ()
processMonkey bananasArray initialSecretNumber = do
  seenArray <- V.new totalHashes :: ST s (MVector s Bool)
  go 0 initialSecretNumber emptyDiffSequence seenArray bananasArray
  where
    go ::
         Int
      -> Int
      -> DifferenceSequence
      -> MVector s Bool
      -> MVector s Int
      -> ST s ()
    go 2000 _ _ _ _ = return ()
    go stepsTaken secretNumber differenceSequence seenArray bananasArray' = do
      let currentBananas = secretNumber `mod` 10
          newSecretNumber = nextSecretNumber secretNumber
          newBananas = newSecretNumber `mod` 10
          newDifference = newBananas - currentBananas
          newDifferenceSequence = shiftSequence newDifference differenceSequence
          newHash = hash newDifferenceSequence
      seenNewHash <- V.read seenArray newHash
      unless (stepsTaken < 4 || seenNewHash)
        $ V.modify bananasArray' (+ newBananas) newHash
      unless (stepsTaken < 4) $ V.write seenArray newHash True
      go
        (stepsTaken + 1)
        newSecretNumber
        newDifferenceSequence
        seenArray
        bananasArray'

processMonkeys :: MVector s Int -> [Int] -> ST s ()
processMonkeys bananasArray = traverse_ (processMonkey bananasArray)

findMaximumBananas :: MVector s Int -> ST s Int
findMaximumBananas bananasArray = go bananasArray 0 0
  where
    go :: MVector s Int -> Int -> Int -> ST s Int
    go bananasArray' n acc = do
      if n == V.length bananasArray'
        then return acc
        else do
          next <- V.read bananasArray' n
          go bananasArray' (n + 1) $ max next acc

optimizeBananas :: [Int] -> Int
optimizeBananas initialSecretNumbers = runST go
  where
    go :: ST s Int
    go = do
      returnsArray <- V.new totalHashes :: ST s (MVector s Int)
      processMonkeys returnsArray initialSecretNumbers
      findMaximumBananas returnsArray
