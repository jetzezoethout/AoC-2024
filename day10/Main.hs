module Main where

import           ProcessFile    (processFile)
import           TopographicMap (parseTopographicMap, totalRating, totalScore)

main :: IO ()
main =
  processFile $ \text -> do
    let topographicMap = parseTopographicMap text
    print $ totalScore topographicMap
    print $ totalRating topographicMap
