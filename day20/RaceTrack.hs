module RaceTrack where

import           Control.Monad (guard)
import           Coordinate    (Coordinate)
import           Data.Map      ((!), (!?))
import qualified Data.Map      as M
import           Data.Maybe    (mapMaybe)
import           Data.Text     (Text)
import           Direction     (allDirections, moveTowards, moveTowardsBy)
import           Grid          (Grid, atCoordinate, parseGrid)
import           LocatedChar   (LocatedChar (..), locateText)

data RaceTrackElement
  = Free
  | Wall
  deriving (Eq, Show)

fromChar :: Char -> RaceTrackElement
fromChar '#' = Wall
fromChar _   = Free

data RaceTrack = RaceTrack
  { track :: Grid RaceTrackElement
  , start :: Coordinate
  , end   :: Coordinate
  } deriving (Show)

parseRaceTrack :: Text -> RaceTrack
parseRaceTrack text =
  RaceTrack
    { track = parseGrid fromChar text
    , start = location $ head $ filter ((== 'S') . char) locatedChars
    , end = location $ head $ filter ((== 'E') . char) locatedChars
    }
  where
    locatedChars = locateText text

type Path = [Coordinate]

findPath :: RaceTrack -> Path
findPath RaceTrack {..} = go start firstStep
  where
    firstStep =
      head
        $ filter ((== Free) . (track `atCoordinate`))
        $ map (start `moveTowards`) allDirections
    go previous current
      | current == end = [previous, current]
      | otherwise =
        let next =
              head
                $ filter ((== Free) . (track `atCoordinate`))
                $ filter (/= previous)
                $ map (current `moveTowards`) allDirections
         in previous : go current next

getCheats :: Path -> [Int]
getCheats path = path >>= cheatAt
  where
    positions = M.fromList $ zip path [0 ..]
    cheatAt coord =
      mapMaybe (cheatTowards coord (positions ! coord)) allDirections
    cheatTowards coord position dir = do
      reappearPosition <- positions !? moveTowardsBy coord dir 2
      let won = reappearPosition - position - 2
      guard $ won > 0
      return won
