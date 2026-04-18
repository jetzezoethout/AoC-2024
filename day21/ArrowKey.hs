module ArrowKey where

import           Coordinate (Coordinate (..))
import           Direction  (Direction (..))
import           KeyPad     (KeyPad (..), paths)

data ArrowKey
  = ArrowKey Direction
  | ApplyKey

instance KeyPad ArrowKey where
  location :: ArrowKey -> Coordinate
  location (ArrowKey North) = Coordinate {row = 0, column = 1}
  location (ArrowKey East)  = Coordinate {row = 1, column = 2}
  location (ArrowKey South) = Coordinate {row = 1, column = 1}
  location (ArrowKey West)  = Coordinate {row = 1, column = 0}
  location ApplyKey         = Coordinate {row = 0, column = 2}
  atLocation :: Coordinate -> Maybe ArrowKey
  atLocation Coordinate {..}
    | (row, column) == (0, 1) = Just $ ArrowKey North
    | (row, column) == (1, 2) = Just $ ArrowKey East
    | (row, column) == (1, 1) = Just $ ArrowKey South
    | (row, column) == (1, 0) = Just $ ArrowKey West
    | (row, column) == (0, 2) = Just ApplyKey
    | otherwise = Nothing
  applyKey :: ArrowKey
  applyKey = ApplyKey

optimalForSequence :: KeyPad a => Int -> [a] -> Int
optimalForSequence numArrowPads keys =
  sum $ (zipWith $ optimalBetweenKeys numArrowPads) (applyKey : keys) keys

optimalBetweenKeys :: KeyPad a => Int -> a -> a -> Int
optimalBetweenKeys numArrowPads from to =
  minimum $ map (optimalForPath $ numArrowPads - 1) $ paths from to

optimalForPath :: Int -> [Direction] -> Int
optimalForPath 0 path = length path + 1
optimalForPath numArrowPads path =
  optimalForSequence numArrowPads (map ArrowKey path <> [ApplyKey])
