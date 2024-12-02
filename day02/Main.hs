module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           Report      (isDampenerSafe, isSafe, parseReport)

main :: IO ()
main =
  processFile $ \text -> do
    let reports = map parseReport $ T.lines text
    print $ length $ filter isSafe reports
    print $ length $ filter isDampenerSafe reports
