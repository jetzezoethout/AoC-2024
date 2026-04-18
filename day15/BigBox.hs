module BigBox where

import           Box         (Box (..))
import           Coordinate  (Coordinate (..))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import           Direction   (Direction (..), moveTowards)
import           LocatedChar (LocatedChar (..), locateTextLines)

newtype BigBox = BigBox
  { leftSide :: Coordinate
  } deriving (Eq, Ord, Show)

parseBigBoxes :: [Text] -> Set BigBox
parseBigBoxes =
  S.fromList
    . map (BigBox . location)
    . filter ((== '[') . char)
    . locateTextLines

instance Box BigBox where
  spacesTowards :: BigBox -> Direction -> [Coordinate]
  spacesTowards BigBox {..} dir =
    case dir of
      North -> map (`moveTowards` North) [leftSide, rightSide]
      South -> map (`moveTowards` South) [leftSide, rightSide]
      West  -> [leftSide `moveTowards` West]
      East  -> [rightSide `moveTowards` East]
    where
      rightSide = leftSide `moveTowards` East
  shift :: BigBox -> Direction -> BigBox
  shift BigBox {..} dir = BigBox $ leftSide `moveTowards` dir
  locate :: Set BigBox -> Coordinate -> Maybe BigBox
  locate boxes coord =
    if BigBox coord `S.member` boxes
      then Just $ BigBox coord
      else let toTheLeft = BigBox (coord `moveTowards` West)
            in if toTheLeft `S.member` boxes
                 then Just toTheLeft
                 else Nothing
  gps :: BigBox -> Int
  gps BigBox {..} = 100 * leftSide.row + leftSide.column
