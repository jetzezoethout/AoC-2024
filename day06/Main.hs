module Main where

import           Area        (foolGuard, parseArea, walk)
import           Guard       (findGuard)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let area = parseArea text
        guard = findGuard text
    print $ walk area guard
    print $ foolGuard area guard
