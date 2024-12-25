module Main where

import           Lock        (fitsInside, parseLocksAndKeys)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let (locks, keys) = parseLocksAndKeys text
    print
      $ length
      $ [(lock, key) | lock <- locks, key <- keys, key `fitsInside` lock]
