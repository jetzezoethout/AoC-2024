module Node where

import           Coordinate (Coordinate)
import           Direction  (Direction (East), allDirections, clockWise,
                             counterClockWise, invert, moveTowards)
import           Maze       (Maze, end, start)

data Node = Node
  { position :: Coordinate
  , facing   :: Direction
  } deriving (Eq, Ord, Show)

turnClockWise :: Node -> Node
turnClockWise Node {..} = Node {position = position, facing = clockWise facing}

turnCounterClockWise :: Node -> Node
turnCounterClockWise Node {..} =
  Node {position = position, facing = counterClockWise facing}

revert :: Node -> Node
revert Node {..} = Node {position = position, facing = invert facing}

forward :: Node -> Node
forward Node {..} =
  Node {position = position `moveTowards` facing, facing = facing}

startNode :: Maze -> Node
startNode maze = Node {position = start maze, facing = East}

endNodes :: Maze -> [Node]
endNodes maze =
  [Node {position = end maze, facing = dir} | dir <- allDirections]
