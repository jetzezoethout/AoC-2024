module Main where

import           Connection      (badWires, evaluate, parseConnections)
import           Data.List       (sort)
import           Data.List.Split (splitOn)
import           Data.Map        ((!))
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           ProcessFile     (processFile)
import           Wire            (parseInputWireSignals)

main :: IO ()
main =
  processFile $ \text -> do
    let sections = splitOn [""] $ T.lines text
        inputWireSignals = parseInputWireSignals $ head sections
        connections = parseConnections $ sections !! 1
    print $ evaluate connections (inputWireSignals !)
    T.putStrLn $ T.intercalate "," $ sort $ badWires connections
