module Robot where

import           Coordinate  (Coordinate (..), addCoordinate)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List   (foldl')
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseInt)

data Robot = Robot
  { position :: Coordinate
  , velocity :: Coordinate
  } deriving (Show)

parseCoordinate :: Text -> Coordinate
parseCoordinate text =
  let parts = T.splitOn "," $ T.splitOn "=" text !! 1
   in Coordinate {row = parseInt $ parts !! 1, column = parseInt $ head parts}

parseRobot :: Text -> Robot
parseRobot text =
  let parts = T.words text
   in Robot
        { position = parseCoordinate $ head parts
        , velocity = parseCoordinate $ parts !! 1
        }

moveRobotOnce :: Robot -> Robot
moveRobotOnce Robot {..} =
  Robot
    { position = correct $ position `addCoordinate` velocity
    , velocity = velocity
    }
  where
    correct Coordinate {..} =
      Coordinate {row = row `mod` 103, column = column `mod` 101}

moveRobotsOnce :: [Robot] -> [Robot]
moveRobotsOnce = map moveRobotOnce

moveRobots :: Int -> [Robot] -> [Robot]
moveRobots steps robots =
  foldl' (flip ($)) robots $ replicate steps moveRobotsOnce

findRowClustering :: [Robot] -> Int
findRowClustering = go
  where
    -- Picture has at least two rows with at least 31 robots (from border)
    go :: [Robot] -> Int
    go robots =
      if length (filter (>= 31) $ M.elems $ rowCounts robots) >= 2
        then 0
        else 1 + go (moveRobotsOnce robots)
    rowCounts :: [Robot] -> IntMap Int
    rowCounts robots = M.fromListWith (+) $ map ((, 1) . row . position) robots

findColumnClustering :: [Robot] -> Int
findColumnClustering = go
  where
    -- Picture has at least two columns with at least 33 robots (from border)
    go :: [Robot] -> Int
    go robots =
      if length (filter (>= 33) $ M.elems $ columnCounts robots) >= 2
        then 0
        else 1 + go (moveRobotsOnce robots)
    columnCounts :: [Robot] -> IntMap Int
    columnCounts robots =
      M.fromListWith (+) $ map ((, 1) . column . position) robots

findChristmasTree :: [Robot] -> Int
findChristmasTree robots =
  let rowRemainder = findRowClustering robots
      columnRemainder = findColumnClustering robots
      -- Solve CRT system using 51 * 101 - 50 * 103 = 1
   in (51 * 101 * rowRemainder - 50 * 103 * columnRemainder) `mod` (101 * 103)
