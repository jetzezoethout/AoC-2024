module Main where

import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Towel           (makeWith, parseTowels)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        parts = splitOn [""] textLines
        towels = parseTowels $ head $ head parts
        patterns = parts !! 1
        waysToMake = map (makeWith towels) patterns
    print $ length $ filter (> 0) waysToMake
    print $ sum waysToMake
