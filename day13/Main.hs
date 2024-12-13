module Main where

import           ClawMachine     (blowUp, parseClawMachine, tokensRequired)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import qualified Data.Text       as T
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let blocks = splitOn [""] $ T.lines text
        clawMachines = map parseClawMachine blocks
        hugeClawMachines = map blowUp clawMachines
    print $ sum $ mapMaybe tokensRequired clawMachines
    print $ sum $ mapMaybe tokensRequired hugeClawMachines
