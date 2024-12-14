module Quadrant where

import           Coordinate (Coordinate (..))

data Quadrant
  = NE
  | NW
  | SE
  | SW
  deriving (Enum)

allQuadrants :: [Quadrant]
allQuadrants = [NE .. SW]

isInQuadrant :: Quadrant -> Coordinate -> Bool
isInQuadrant quadrant Coordinate {..} =
  case quadrant of
    NE -> row < 51 && column > 50
    NW -> row < 51 && column < 50
    SE -> row > 51 && column < 50
    SW -> row > 51 && column > 50

countInQuadrant :: [Coordinate] -> Quadrant -> Int
countInQuadrant coordinates quadrant =
  length $ filter (isInQuadrant quadrant) coordinates
