module Dimensions where

import           Coordinate (Coordinate (..))
import           Data.Text  (Text)
import qualified Data.Text  as T

data Dimensions = Dimensions
  { width  :: Int
  , height :: Int
  }

parseDimensions :: [Text] -> Dimensions
parseDimensions textLines =
  Dimensions {width = T.length $ head textLines, height = length textLines}

isInBounds :: Dimensions -> Coordinate -> Bool
isInBounds Dimensions {..} Coordinate {..} =
  0 <= row && row < height && 0 <= column && column < width
