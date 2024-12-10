module TopographicMap where

import           Coordinate (Coordinate)
import           Data.Char  (digitToInt)
import           Data.List  (nub)
import           Data.Text  (Text)
import           Direction  (allDirections, moveTowards)
import           Grid       (Grid, allCoordinates, atCoordinate, parseGrid,
                             safeAtCoordinate)

type TopographicMap = Grid Int

parseTopographicMap :: Text -> TopographicMap
parseTopographicMap = parseGrid digitToInt

walk :: TopographicMap -> Coordinate -> [Coordinate]
walk topographicMap start = go [start]
  where
    neighbours position = map (position `moveTowards`) allDirections
    go [] = []
    go (position:toGo) =
      let height = topographicMap `atCoordinate` position
       in if height == 9
            then position : go toGo
            else go
                   $ filter
                       ((== Just (height + 1))
                          . (topographicMap `safeAtCoordinate`))
                       (neighbours position)
                       <> toGo

trailHeads :: TopographicMap -> [Coordinate]
trailHeads topographicMap =
  filter ((== 0) . (topographicMap `atCoordinate`))
    $ allCoordinates topographicMap

score :: TopographicMap -> Coordinate -> Int
score topographicMap start = length $ nub $ walk topographicMap start

totalScore :: TopographicMap -> Int
totalScore topographicMap =
  sum $ map (score topographicMap) $ trailHeads topographicMap

rating :: TopographicMap -> Coordinate -> Int
rating topographicMap start = length $ walk topographicMap start

totalRating :: TopographicMap -> Int
totalRating topographicMap =
  sum $ map (rating topographicMap) $ trailHeads topographicMap
