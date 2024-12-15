module Warehouse where

import           Coordinate  (Coordinate (..))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Direction
import           Grid
import           LocatedChar (LocatedChar (..), locateTextLines)

data WarehouseElement
  = Wall
  | Floor
  deriving (Eq, Show)

fromChar :: Char -> WarehouseElement
fromChar '#' = Wall
fromChar _   = Floor

type Warehouse = Grid WarehouseElement

parseWarehouse :: [Text] -> Warehouse
parseWarehouse = parseGridLines fromChar

type Boxes = Set Coordinate

parseBoxes :: [Text] -> Boxes
parseBoxes =
  S.fromList . map location . filter ((== 'O') . char) . locateTextLines

sumGPS :: Set Coordinate -> Int
sumGPS = sum . S.map getGPS
  where
    getGPS Coordinate {..} = 100 * row + column

type Robot = Coordinate

findRobot :: [Text] -> Robot
findRobot = location . head . filter ((== '@') . char) . locateTextLines

parseMove :: Char -> Direction
parseMove '^' = North
parseMove '>' = East
parseMove 'v' = South
parseMove '<' = West
parseMove _   = error "not a direction"

parseMoves :: [Text] -> [Direction]
parseMoves textLines = textLines >>= map parseMove . T.unpack

findFreeTile :: Warehouse -> Boxes -> Robot -> Direction -> Maybe Coordinate
findFreeTile warehouse boxes robot move = go $ robot `moveTowards` move
  where
    go current =
      if current `S.member` boxes
        then go $ current `moveTowards` move
        else case warehouse `atCoordinate` current of
               Floor -> Just current
               Wall  -> Nothing

doMove :: Warehouse -> Boxes -> Robot -> Direction -> (Boxes, Robot)
doMove warehouse boxes robot move =
  case findFreeTile warehouse boxes robot move of
    Nothing -> (boxes, robot)
    Just free ->
      let updatedRobot = robot `moveTowards` move
          updatedBoxes =
            if updatedRobot == free
              then boxes
              else S.insert free $ S.delete updatedRobot boxes
       in (updatedBoxes, updatedRobot)

doMoves :: Warehouse -> Boxes -> Robot -> [Direction] -> (Boxes, Robot)
doMoves wareHouse = go
  where
    go boxes robot [] = (boxes, robot)
    go boxes robot (move:remaining) =
      let (updatedBoxes, updatedRobot) = doMove wareHouse boxes robot move
       in go updatedBoxes updatedRobot remaining
