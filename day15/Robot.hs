module Robot where

import           Coordinate  (Coordinate)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Direction   (Direction (..))
import           LocatedChar (LocatedChar (..), locateTextLines)

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
