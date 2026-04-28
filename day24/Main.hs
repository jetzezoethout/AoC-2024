module Main where

import           Data.List   (foldl')
import           ProcessFile (processFile)
import           Wire        (evaluate, parseWireMap, zWires)

main :: IO ()
main =
  processFile $ \text -> do
    let wireMap = parseWireMap text
    print $ fromBinary $ map (evaluate wireMap) $ zWires wireMap

fromBinary :: [Int] -> Int
fromBinary = foldl' (\x y -> 2 * x + y) 0
