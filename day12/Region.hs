module Region where

import           Coordinate (Coordinate)
import           Data.Set   (Set, (\\))
import qualified Data.Set   as S
import           Data.Text  (Text)
import           Direction  (allDirections, moveTowards)
import           Grid       (Grid, allCoordinates, atCoordinate, parseGrid,
                             safeAtCoordinate)

type Garden = Grid Char

parseGarden :: Text -> Garden
parseGarden = parseGrid id

type Region = Set Coordinate

separateRegions :: Garden -> [Region]
separateRegions garden = go $ S.fromList $ allCoordinates garden
  where
    go toSeparate =
      case S.lookupMin toSeparate of
        Nothing -> []
        Just start ->
          let newRegion = floodfill garden start
           in newRegion : go (toSeparate \\ newRegion)

floodfill :: Garden -> Coordinate -> Region
floodfill garden start = go (S.singleton start) [start]
  where
    plant = garden `atCoordinate` start
    neighbours position = map (position `moveTowards`) allDirections
    go seen [] = seen
    go seen (next:remaining) =
      let newPlots =
            filter (`S.notMember` seen)
              $ filter ((== Just plant) . (garden `safeAtCoordinate`))
              $ neighbours next
       in go (foldr S.insert seen newPlots) $ newPlots <> remaining
