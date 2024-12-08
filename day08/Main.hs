module Main where

import           Antennas    (getAntinodes, getHarmonicAntinodes, parseAntennas)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let antennas = parseAntennas text
    print $ length $ getAntinodes antennas
    print $ length $ getHarmonicAntinodes antennas
