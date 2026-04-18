module SmallBox where

import           Box         (Box (..))
import           Coordinate  (Coordinate (..))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import           Direction   (Direction, moveTowards)
import           LocatedChar (LocatedChar (..), locateTextLines)

newtype SmallBox = SmallBox
  { boxAt :: Coordinate
  } deriving (Show, Eq, Ord)

parseSmallBoxes :: [Text] -> Set SmallBox
parseSmallBoxes =
  S.fromList
    . map (SmallBox . location)
    . filter ((== 'O') . char)
    . locateTextLines

instance Box SmallBox where
  spacesTowards :: SmallBox -> Direction -> [Coordinate]
  spacesTowards SmallBox {..} dir = [boxAt `moveTowards` dir]
  shift :: SmallBox -> Direction -> SmallBox
  shift SmallBox {..} dir = SmallBox $ boxAt `moveTowards` dir
  locate :: Set SmallBox -> Coordinate -> Maybe SmallBox
  locate boxes coord = [SmallBox coord | SmallBox coord `S.member` boxes]
  gps :: SmallBox -> Int
  gps SmallBox {..} = 100 * boxAt.row + boxAt.column
