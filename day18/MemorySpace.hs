module MemorySpace where

import           Coordinate    (Coordinate (..))
import           Data.Maybe    (isJust)
import           Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Direction     (allDirections, moveTowards)
import           Parsers       (parseUnsignedInt)

type Byte = Coordinate

parseByte :: Text -> Byte
parseByte textLine =
  let parts = T.splitOn "," textLine
   in Coordinate
        { row = parseUnsignedInt $ parts !! 1
        , column = parseUnsignedInt $ head parts
        }

type MemorySpace = Set Byte

size :: Int
size = 71

isFree :: MemorySpace -> Coordinate -> Bool
isFree space coord =
  0 <= coord.row
    && coord.row < size
    && 0 <= coord.column
    && coord.column < size
    && coord `S.notMember` space

isExit :: Coordinate -> Bool
isExit coord = coord.row == size - 1 && coord.column == size - 1

afterNanoSeconds :: [Byte] -> Int -> MemorySpace
afterNanoSeconds bytes t = S.fromList $ take t bytes

data ShortestDistance = ShortestDistance
  { target   :: Coordinate
  , distance :: Int
  }

findExit :: MemorySpace -> Maybe Int
findExit space =
  go
    (S.singleton start)
    (Seq.singleton ShortestDistance {target = start, distance = 0})
  where
    start = Coordinate 0 0
    go _ Empty = Nothing
    go seen (ShortestDistance {..} :<| remaining) =
      if isExit target
        then Just distance
        else let newTargets =
                   filter (`S.notMember` seen)
                     $ filter (isFree space)
                     $ map (target `moveTowards`) allDirections
                 toEnqueue =
                   [ ShortestDistance
                     {distance = distance + 1, target = newTarget}
                   | newTarget <- newTargets
                   ]
              in go (seen `S.union` S.fromList newTargets)
                   $ remaining >< Seq.fromList toEnqueue

findExitAfterNanoseconds :: [Byte] -> Int -> Maybe Int
findExitAfterNanoseconds bytes t = findExit $ afterNanoSeconds bytes t

findFirstBlockade :: [Byte] -> Byte
findFirstBlockade bytes = bytes !! (go 1024 (length bytes) - 1)
  where
    go left right
      | right - left == 1 = right
      | otherwise =
        let probe = (left + right) `div` 2
         in if isJust $ findExitAfterNanoseconds bytes probe
              then go probe right
              else go left probe
