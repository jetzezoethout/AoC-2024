module Main where

import           Disk           (checkSum, compactify)
import           Fragmentable   (parseFragmentableDisc)
import           ProcessFile    (processFile)
import           Unfragmentable (parseUnfragmentableDisk)

main :: IO ()
main =
  processFile $ \text -> do
    let fragmentableDisk = parseFragmentableDisc text
        unfragmentableDisk = parseUnfragmentableDisk text
    print $ checkSum $ compactify fragmentableDisk
    print $ checkSum $ compactify unfragmentableDisk
