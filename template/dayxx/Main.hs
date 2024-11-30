module Main where

import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    putStrLn "TODO: solve puzzle"
