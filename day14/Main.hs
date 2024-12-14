module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           Quadrant    (allQuadrants, countInQuadrant)
import           Robot       (Robot (position), findChristmasTree, moveRobots,
                              parseRobot)

main :: IO ()
main =
  processFile $ \text -> do
    let robots = map parseRobot $ T.lines text
        after100Steps = map position $ moveRobots 100 robots
    print $ product $ map (countInQuadrant after100Steps) allQuadrants
    print $ findChristmasTree robots
