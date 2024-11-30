module Coordinate where

data Coordinate = Coordinate
  { row    :: Int
  , column :: Int
  } deriving (Eq, Ord, Show)

addCoordinate :: Coordinate -> Coordinate -> Coordinate
c1 `addCoordinate` c2 =
  Coordinate {row = c1.row + c2.row, column = c1.column + c2.column}

relativeTo :: Coordinate -> Coordinate -> Coordinate
coord `relativeTo` origin =
  Coordinate
    {row = coord.row - origin.row, column = coord.column - origin.column}

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance c1 c2 = abs (c1.row - c2.row) + abs (c1.column - c2.column)

determinant :: Coordinate -> Coordinate -> Int
determinant c1 c2 = c1.row * c2.column - c1.column * c2.row

dilate :: Int -> Coordinate -> Coordinate
dilate factor Coordinate {..} =
  Coordinate {row = factor * row, column = factor * column}
