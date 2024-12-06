module Guard where

import           Coordinate  (Coordinate (..))
import           Data.Maybe  (mapMaybe)
import           Data.Text   (Text)
import           Direction   (Direction (..), clockWise, moveTowards)
import           LocatedChar (LocatedChar (..), locateText)

data Guard = Guard
  { position :: Coordinate
  , facing   :: Direction
  } deriving (Eq, Ord, Show)

parseDirection :: Char -> Maybe Direction
parseDirection '^' = Just North
parseDirection '>' = Just East
parseDirection 'v' = Just South
parseDirection '<' = Just West
parseDirection _   = Nothing

findGuard :: Text -> Guard
findGuard text = head $ mapMaybe findGuardAt $ locateText text
  where
    findGuardAt LocatedChar {..} = Guard location <$> parseDirection char

inFrontOf :: Guard -> Coordinate
inFrontOf Guard {..} = position `moveTowards` facing

forward :: Guard -> Guard
forward guard = guard {position = inFrontOf guard}

turn :: Guard -> Guard
turn Guard {..} = Guard {position = position, facing = clockWise facing}
