module Maze where

import           Coordinate (Coordinate (..))
import           Data.Text  (Text)
import           Grid       (Grid (height, width), parseGrid)

data MazeElement
  = Free
  | Wall
  deriving (Eq)

fromChar :: Char -> MazeElement
fromChar '#' = Wall
fromChar _   = Free

type Maze = Grid MazeElement

parseMaze :: Text -> Grid MazeElement
parseMaze = parseGrid fromChar

start :: Maze -> Coordinate
start maze = Coordinate {row = maze.height - 2, column = 1}

end :: Maze -> Coordinate
end maze = Coordinate {row = 1, column = maze.width - 2}
