module Main where

import           Data.Map    ((!))
import qualified Data.Map    as M
import           Maze        (parseMaze)
import           Move        (shortestDistances)
import           Node        (Node (..), endNodes, revert, startNode)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let maze = parseMaze text
        distancesFromStart = shortestDistances maze [startNode maze]
        distancesToEnd =
          M.mapKeys revert $ shortestDistances maze $ endNodes maze
        shortestDistance = distancesToEnd ! startNode maze
    print shortestDistance
    print
      $ M.size
      $ M.mapKeys position
      $ M.filter (== shortestDistance)
      $ M.unionWith (+) distancesFromStart distancesToEnd
