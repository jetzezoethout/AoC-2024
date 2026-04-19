module RaceTrack where

import           Control.Monad (guard)
import           Coordinate    (Coordinate (..), addCoordinate,
                                manhattanDistance)
import           Data.Map      (Map, (!?))
import qualified Data.Map      as M
import           Data.Maybe    (maybeToList)
import           Data.Text     (Text)
import           Direction     (allDirections, moveTowards)
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

type PathMap = Map Coordinate Int

getPathMap :: Path -> PathMap
getPathMap path = M.fromList $ zip path [0 ..]

manhattanDisc :: Coordinate -> Int -> [Coordinate]
manhattanDisc center distance =
  [ center `addCoordinate` Coordinate row column
  | row <- [-distance .. distance]
  , column <- [abs row - distance .. distance - abs row]
  ]

getCheats :: PathMap -> Int -> [Int]
getCheats pathMap maxShortcut =
  [ cheat
  | start <- M.keys pathMap
  , end <- manhattanDisc start maxShortcut
  , cheat <- maybeToList $ getCheat start end
  ]
  where
    getCheat start end = do
      startIndex <- pathMap !? start
      endIndex <- pathMap !? end
      let shortcutLength = manhattanDistance start end
      guard $ startIndex + shortcutLength < endIndex
      return $ endIndex - startIndex - shortcutLength
