module Lock where

import           Coordinate      (Coordinate (..))
import           Data.Either     (partitionEithers)
import           Data.List.Split (splitOn)
import           Data.Map        ((!))
import qualified Data.Map        as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import           LocatedChar     (LocatedChar (..), locateTextLines)

newtype Lock = Lock
  { lockHeights :: [Int]
  } deriving (Show)

newtype Key = Key
  { keyHeights :: [Int]
  } deriving (Show)

fitsInside :: Key -> Lock -> Bool
Key {..} `fitsInside` Lock {..} =
  all (<= 5) $ zipWith (+) lockHeights keyHeights

parseLocksAndKeys :: Text -> ([Lock], [Key])
parseLocksAndKeys text =
  let parts = splitOn [""] $ T.lines text
   in partitionEithers $ map parseLockOrKey parts

parseLockOrKey :: [Text] -> Either Lock Key
parseLockOrKey textLines =
  if schematics ! Coordinate 0 0 == '#'
    then Left $ Lock heights
    else Right $ Key heights
  where
    schematics =
      M.fromList $ map (location &&& char) $ locateTextLines textLines
    squaresIn column = [Coordinate row column | row <- [0 .. 6]]
    heightOf column =
      length (filter ((== '#') . (schematics !)) $ squaresIn column) - 1
    heights = [heightOf column | column <- [0 .. 4]]

(&&&) :: (t -> a) -> (t -> b) -> t -> (a, b)
(&&&) f g x = (f x, g x)
