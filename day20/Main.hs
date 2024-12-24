module Main where

import           ProcessFile (processFile)
import           RaceTrack   (findPath, getCheats, parseRaceTrack)

main :: IO ()
main =
  processFile $ \text -> do
    let raceTrack = parseRaceTrack text
        path = findPath raceTrack
        cheats = getCheats path
    print $ length $ filter (>= 100) cheats
