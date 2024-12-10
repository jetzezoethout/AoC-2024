module Main where

import           ProcessFile (processFile)
import           Stones      (blink, parseStones, totalStones)

main :: IO ()
main =
  processFile $ \text -> do
    let stones = parseStones text
    print $ totalStones $ blink stones 25
    print $ totalStones $ blink stones 75
