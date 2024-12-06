module Main where

import           Area        (isSuitableObstacle, parseArea, visited)
import           Data.List   (delete)
import           Guard       (Guard (position), findGuard)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let area = parseArea text
        guard = findGuard text
        visitedLocations = visited area guard
    print $ length visitedLocations
    print
      $ length
      $ filter (isSuitableObstacle area guard)
      $ delete (position guard) visitedLocations
