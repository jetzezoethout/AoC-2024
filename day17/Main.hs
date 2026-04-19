module Main where

import           Computer        (findLeastFixPoint, parseComputer,
                                  parseProgram, run)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        parts = splitOn [""] textLines
        computer = parseComputer $ head parts
        program = parseProgram $ head $ parts !! 1
    putStrLn $ intercalate "," $ map show $ run program computer
    print $ findLeastFixPoint program computer
