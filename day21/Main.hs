module Main where

import qualified Data.Text   as T
import           NumericKey  (complexity, parseSequence)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let numericalSequences = map parseSequence $ T.lines text
    print $ sum $ map complexity numericalSequences
