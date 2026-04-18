module Box where

import           Coordinate (Coordinate)
import           Data.List  (foldl')
import           Data.Set   (Set, (\\))
import qualified Data.Set   as S
import           Direction  (Direction, moveTowards)
import           Grid       (atCoordinate)
import           Robot      (Robot)
import           Warehouse  (Warehouse, WarehouseElement (..))

class Ord a =>
      Box a
  where
  spacesTowards :: a -> Direction -> [Coordinate]
  shift :: a -> Direction -> a
  locate :: Set a -> Coordinate -> Maybe a
  gps :: a -> Int

data FloorState a = FloorState
  { robot :: Robot
  , boxes :: Set a
  }

getTotalGps :: Box a => Warehouse -> Robot -> Set a -> [Direction] -> Int
getTotalGps warehouse robot initialBoxes moves =
  sum $ map gps $ S.toList finalBoxes
  where
    finalBoxes =
      boxes
        $ doMoves
            warehouse
            FloorState {robot = robot, boxes = initialBoxes}
            moves

doMoves :: Box a => Warehouse -> FloorState a -> [Direction] -> FloorState a
doMoves warehouse = foldl' (doMove warehouse)

doMove :: Box a => Warehouse -> FloorState a -> Direction -> FloorState a
doMove warehouse floorState@FloorState {..} dir =
  case shiftable warehouse dir floorState of
    Nothing -> floorState
    Just toShift ->
      FloorState
        {robot = robot `moveTowards` dir, boxes = shiftBoxes toShift dir boxes}

shiftBoxes :: Box a => Set a -> Direction -> Set a -> Set a
shiftBoxes toShift dir boxes =
  (boxes \\ toShift) `S.union` S.map (`shift` dir) toShift

shiftable :: Box a => Warehouse -> Direction -> FloorState a -> Maybe (Set a)
shiftable warehouse dir FloorState {..} = go S.empty [robot `moveTowards` dir]
  where
    go collected [] = Just collected
    go collected (current:others) =
      case locate boxes current of
        Nothing ->
          case warehouse `atCoordinate` current of
            Floor -> go collected others
            Wall  -> Nothing
        Just box ->
          if box `S.member` collected
            then go collected others
            else go (S.insert box collected) $ spacesTowards box dir <> others
