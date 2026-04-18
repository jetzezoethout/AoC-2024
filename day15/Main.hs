module Main where

import           BigBox          (parseBigBoxes)
import           Box             (getTotalGps)
import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Robot           (findRobot, parseMoves)
import           SmallBox        (parseSmallBoxes)
import           Warehouse       (blowUp, parseWarehouse)

main :: IO ()
main =
  processFile $ \text -> do
    let parts = splitOn [""] $ T.lines text
        warehousePart = head parts
        moves = parseMoves $ parts !! 1
        warehouse = parseWarehouse warehousePart
        smallBoxes = parseSmallBoxes warehousePart
        robot = findRobot warehousePart
    print $ getTotalGps warehouse robot smallBoxes moves
    let bigMap = map blowUp warehousePart
        bigWarehouse = parseWarehouse bigMap
        bigBoxes = parseBigBoxes bigMap
        bigRobot = findRobot bigMap
    print $ getTotalGps bigWarehouse bigRobot bigBoxes moves
