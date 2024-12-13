module Main where

import           FencedRegion (discountedPrice, price, putFence)
import           ProcessFile  (processFile)
import           Region       (parseGarden, separateRegions)

main :: IO ()
main =
  processFile $ \text -> do
    let garden = parseGarden text
        regions = separateRegions garden
        fencedRegions = map putFence regions
    print $ sum $ map price fencedRegions
    print $ sum $ map discountedPrice fencedRegions
