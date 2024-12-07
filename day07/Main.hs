module Main where

import qualified Data.Text   as T
import           Equation    (Equation (testValue), canBeTrue,
                              canBeTrueWithConcatenation, parseEquation)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let equations = map parseEquation $ T.lines text
    print $ sum $ map testValue $ filter canBeTrue equations
    print $ sum $ map testValue $ filter canBeTrueWithConcatenation equations
