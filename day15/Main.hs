module Main where

import           BigWarehouse    (blowUp, collectBoxes, doBigMoves, sumBigGPS)
import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Warehouse       (doMoves, findRobot, parseBoxes, parseMoves,
                                  parseWarehouse, sumGPS)

main :: IO ()
main =
  processFile $ \text -> do
    let parts = splitOn [""] $ T.lines text
        warehousePart = head parts
        moves = parseMoves $ parts !! 1
        warehouse = parseWarehouse warehousePart
        boxes_ = parseBoxes warehousePart
        robot = findRobot warehousePart
        (finalBoxes, finalRobot) = doMoves warehouse boxes_ robot moves
    print $ sumGPS finalBoxes
    let bigMap = map blowUp warehousePart
        bigWarehouse = parseWarehouse bigMap
        bigBoxes = collectBoxes bigMap
        bigRobot = findRobot bigMap
        (finalBigRobot, finalBigBoxes) =
          doBigMoves bigWarehouse bigBoxes bigRobot moves
    -- mapM_ TIO.putStrLn bigMap
    -- print $ bigWarehouse `atCoordinate` Coordinate 4 18
    -- mapM_ print $ boxes finalBigBoxes
    -- putStrLn ""
    -- print finalBigRobot
    print $ sumBigGPS finalBigBoxes
    -- print $ length moves
