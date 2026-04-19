module KeyPad where

import           Coordinate (Coordinate (..))
import           Data.Maybe (maybeToList)
import           Direction  (Direction (..), moveTowards)

class KeyPad a where
  location :: a -> Coordinate
  atLocation :: Coordinate -> Maybe a
  applyKey :: a

tryMove :: KeyPad a => Direction -> a -> Maybe a
tryMove dir key = atLocation $ location key `moveTowards` dir

type Path = [Direction]

paths :: KeyPad a => a -> a -> [Path]
paths from to =
  if fromLocation == toLocation
    then [[]]
    else horizontalStarts <> verticalStarts
  where
    fromLocation = location from
    toLocation = location to
    horizontalStarts =
      case fromLocation.column `compare` toLocation.column of
        EQ -> []
        LT -> do
          next <- maybeToList $ tryMove East from
          pathTail <- paths next to
          return $ East : pathTail
        GT -> do
          next <- maybeToList $ tryMove West from
          pathTail <- paths next to
          return $ West : pathTail
    verticalStarts =
      case fromLocation.row `compare` toLocation.row of
        EQ -> []
        LT -> do
          next <- maybeToList $ tryMove South from
          pathTail <- paths next to
          return $ South : pathTail
        GT -> do
          next <- maybeToList $ tryMove North from
          pathTail <- paths next to
          return $ North : pathTail
