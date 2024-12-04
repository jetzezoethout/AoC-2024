module Main where

import           ProcessFile (processFile)
import           WordSearch  (findCrossMas, findXmas, parseWordSearch)

main :: IO ()
main =
  processFile $ \text -> do
    let wordSearch = parseWordSearch text
    print $ findXmas wordSearch
    print $ findCrossMas wordSearch
