module Main where

import           Data.List       (partition)
import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           PageNumbers     (isInOrder, middle, order, parsePageNumbers)
import           ProcessFile     (processFile)
import           Rule            (parseRules)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        parts = splitOn [""] textLines
        rules = parseRules $ head parts
        updates = map parsePageNumbers $ parts !! 1
        (inOrder, notInOrder) = partition (isInOrder rules) updates
    print $ sum $ map middle inOrder
    print $ sum $ map (middle . order rules) notInOrder
