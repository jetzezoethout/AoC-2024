module ArrowKey where

import           Cached        (Cached, runMemoized, withCache)
import           Control.Monad (zipWithM)
import           Coordinate    (Coordinate (..))
import           Direction     (Direction (..))
import           KeyPad        (KeyPad (..), Path, paths)

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

data ResolutionData = ResolutionData
  { numArrowPads :: Int
  , path         :: Path
  } deriving (Eq, Ord)

shortestHumanLength :: KeyPad a => Int -> [a] -> Int
shortestHumanLength humanPadIndex targetSequence =
  runMemoized $ optimalForSequence humanPadIndex targetSequence
  where
    optimalForSequence :: KeyPad a => Int -> [a] -> Cached ResolutionData Int
    optimalForSequence numArrowPads keys = do
      subOptima <-
        zipWithM (optimalBetweenKeys numArrowPads) (applyKey : keys) keys
      return $ sum subOptima
    optimalBetweenKeys :: KeyPad a => Int -> a -> a -> Cached ResolutionData Int
    optimalBetweenKeys numArrowPads from to = do
      possibilities <- mapM (optimalForPath $ numArrowPads - 1) (paths from to)
      return $ minimum possibilities
    optimalForPath :: Int -> Path -> Cached ResolutionData Int
    optimalForPath numArrowPads path = withCache resolve ResolutionData {..}
    resolve :: ResolutionData -> Cached ResolutionData Int
    resolve ResolutionData {..} =
      if numArrowPads == 0
        then return (length path + 1)
        else optimalForSequence numArrowPads $ map ArrowKey path <> [ApplyKey]
