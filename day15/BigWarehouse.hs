module BigWarehouse where

import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import           Control.Monad.State  (State, gets, modify, runState)
import           Coordinate           (Coordinate (..))
import           Data.List            (foldl')
import           Data.Set             (Set, (\\))
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Direction            (Direction (..), moveTowards)
import           Grid                 (atCoordinate)
import           LocatedChar          (LocatedChar (..), locateTextLines)
import           Warehouse            (Robot, Warehouse, WarehouseElement (..))

blowUp :: Text -> Text
blowUp =
  T.replace "#" "##"
    . T.replace "@" "@."
    . T.replace "." ".."
    . T.replace "O" "[]"

newtype BigBox = BigBox
  { leftSide :: Coordinate
  } deriving (Eq, Ord, Show)

spaceTowards :: BigBox -> Direction -> [Coordinate]
BigBox {..} `spaceTowards` dir =
  case dir of
    North -> map (`moveTowards` North) [leftSide, rightSide]
    South -> map (`moveTowards` South) [leftSide, rightSide]
    West  -> [leftSide `moveTowards` West]
    East  -> [rightSide `moveTowards` East]
  where
    rightSide = leftSide `moveTowards` East

shiftTowards :: BigBox -> Direction -> BigBox
BigBox {..} `shiftTowards` dir = BigBox $ leftSide `moveTowards` dir

bigGPS :: BigBox -> Int
bigGPS BigBox {..} = calcGPS leftSide
  where
    calcGPS Coordinate {..} = 100 * row + column

newtype BigBoxes = BigBoxes
  { boxes :: Set BigBox
  } deriving (Show)

collectBoxes :: [Text] -> BigBoxes
collectBoxes =
  BigBoxes
    . S.fromList
    . map (BigBox . location)
    . filter ((== '[') . char)
    . locateTextLines

getAt :: BigBoxes -> Coordinate -> Maybe BigBox
BigBoxes {..} `getAt` coord =
  if BigBox coord `S.member` boxes
    then Just $ BigBox coord
    else let toTheLeft = BigBox (coord `moveTowards` West)
          in if toTheLeft `S.member` boxes
               then Just toTheLeft
               else Nothing

shiftBoxesTowards :: Set BigBox -> Direction -> BigBoxes -> BigBoxes
shiftBoxesTowards toShift dir BigBoxes {..} =
  BigBoxes $ (boxes \\ toShift) `S.union` S.map (`shiftTowards` dir) toShift

shiftBoxTowards :: BigBox -> Direction -> BigBoxes -> BigBoxes
shiftBoxTowards bigBox dir =
  BigBoxes . S.insert (bigBox `shiftTowards` dir) . S.delete bigBox . boxes

sumBigGPS :: BigBoxes -> Int
sumBigGPS BigBoxes {..} = sum $ S.map bigGPS boxes

type Context = ReaderT Warehouse (State BigBoxes)

data ShiftingTree
  = Leaf
      { target :: Coordinate
      }
  | Box
      { toShift        :: BigBox
      , pushingAgainst :: [ShiftingTree]
      }

data ShiftResult
  = Moveable
  | Blocked
  deriving (Eq)

instance Semigroup ShiftResult where
  (<>) :: ShiftResult -> ShiftResult -> ShiftResult
  Moveable <> Moveable = Moveable
  _ <> _               = Blocked

instance Monoid ShiftResult where
  mempty :: ShiftResult
  mempty = Moveable

fromFloorPlan :: WarehouseElement -> ShiftResult
fromFloorPlan Floor = Moveable
fromFloorPlan Wall  = Blocked

resolve :: ShiftingTree -> Context ShiftResult
resolve = go
  where
    go :: ShiftingTree -> Context ShiftResult
    go Leaf {..} = asks $ fromFloorPlan . (`atCoordinate` target)
    go Box {..}  = mconcat <$> traverse go pushingAgainst

getBoxes :: ShiftingTree -> Set BigBox
getBoxes = go
  where
    go (Leaf _) = S.empty
    go Box {..} = foldl' S.union (S.singleton toShift) $ map go pushingAgainst

shiftAll :: Direction -> ShiftingTree -> Context ()
shiftAll dir tree = modify $ shiftBoxesTowards (getBoxes tree) dir

buildTree :: Robot -> Direction -> Context ShiftingTree
buildTree robot move = go $ robot `moveTowards` move
  where
    go :: Coordinate -> Context ShiftingTree
    go current = do
      boxHere <- gets (`getAt` current)
      case boxHere of
        Nothing -> return $ Leaf {target = current}
        Just toShift -> do
          pushingAgainst <- traverse go $ toShift `spaceTowards` move
          return Box {..}

doMove :: Robot -> Direction -> Context Robot
doMove robot move = do
  tree <- buildTree robot move
  result <- resolve tree
  case result of
    Moveable -> shiftAll move tree >> return (robot `moveTowards` move)
    Blocked  -> return robot

doBigMoves :: Warehouse -> BigBoxes -> Robot -> [Direction] -> (Robot, BigBoxes)
doBigMoves wareHouse bigBoxes robot moves =
  runState (runReaderT (go robot moves) wareHouse) bigBoxes
  where
    go :: Robot -> [Direction] -> Context Robot
    go current [] = return current
    go current (move:remaining) = do
      nextRobot <- doMove current move
      go nextRobot remaining
