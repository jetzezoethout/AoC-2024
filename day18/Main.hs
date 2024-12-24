module Main where

import           Coordinate  (Coordinate (..))
import           Data.Maybe  (fromJust)
import qualified Data.Text   as T
import           MemorySpace (findExitAfterNanoseconds, findFirstBlockade,
                              parseByte)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let bytes = map parseByte $ T.lines text
        firstBlockade = findFirstBlockade bytes
    print $ fromJust $ findExitAfterNanoseconds bytes 1024
    putStrLn $ show firstBlockade.column <> "," <> show firstBlockade.row
