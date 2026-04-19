module Main where

import           ProcessFile (processFile)
import           RaceTrack   (findPath, getCheats, getPathMap, parseRaceTrack)

main :: IO ()
main =
  processFile $ \text -> do
    let raceTrack = parseRaceTrack text
        path = findPath raceTrack
        pathMap = getPathMap path
    print $ length $ filter (>= 100) $ getCheats pathMap 2
    print $ length $ filter (>= 100) $ getCheats pathMap 20
