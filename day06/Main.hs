module Main where

import           Area        (foolGuard, parseArea, visitedLocations)
import           Guard       (findGuard)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let area = parseArea text
        guard = findGuard text
    print $ visitedLocations area guard
    print $ foolGuard area guard
