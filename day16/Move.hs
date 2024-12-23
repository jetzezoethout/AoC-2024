module Move where

import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.PSQueue (Binding (..), PSQ)
import qualified Data.PSQueue as Q
import           Grid         (atCoordinate)
import           Maze         (Maze, MazeElement (Free))
import           Node         (Node (..), forward, turnClockWise,
                               turnCounterClockWise)

data Move = Move
  { target :: Node
  , cost   :: Int
  }

getMoves :: Maze -> Node -> [Move]
getMoves maze node =
  [ Move {target = forward node, cost = 1}
  | maze `atCoordinate` position (forward node) == Free
  ]
    <> [ Move {target = turnClockWise node, cost = 1000}
       , Move {target = turnCounterClockWise node, cost = 1000}
       ]

shortestDistances :: Maze -> [Node] -> Map Node Int
shortestDistances maze starts =
  go M.empty $ Q.fromList [node :-> 0 | node <- starts]
  where
    go :: Map Node Int -> PSQ Node Int -> Map Node Int
    go distances queue =
      case Q.minView queue of
        Nothing -> distances
        Just (nextNode :-> distance, remaining) ->
          let toUpdate =
                filter ((`M.notMember` distances) . target)
                  $ getMoves maze nextNode
           in go (M.insert nextNode distance distances)
                $ foldr
                    (\Move {..} -> Q.insertWith min target (distance + cost))
                    remaining
                    toUpdate
