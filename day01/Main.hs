module Main where

import           Locations   (parseLocations, similarityScore, totalDistance)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let (locations1, locations2) = parseLocations text
    print $ totalDistance locations1 locations2
    print $ similarityScore locations1 locations2
