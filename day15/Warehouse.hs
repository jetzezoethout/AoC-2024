module Warehouse where

import           Coordinate  (Coordinate (..))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import qualified Data.Text   as T
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

blowUp :: Text -> Text
blowUp =
  T.replace "#" "##"
    . T.replace "@" "@."
    . T.replace "." ".."
    . T.replace "O" "[]"
