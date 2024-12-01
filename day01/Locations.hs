module Locations where

import           Data.List (group, sort)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

newtype Locations = Locations
  { sortedIds :: [Int]
  }

makeLocations :: [Int] -> Locations
makeLocations = Locations . sort

parseLocations :: Text -> (Locations, Locations)
parseLocations text = (makeLocations ids1, makeLocations ids2)
  where
    (ids1, ids2) = unzip $ map parseLocationsLine $ T.lines text
    parseLocationsLine textLine =
      let parts = T.words textLine
       in (parseUnsignedInt $ head parts, parseUnsignedInt $ parts !! 1)

totalDistance :: Locations -> Locations -> Int
totalDistance (Locations sortedIds1) (Locations sortedIds2) =
  sum $ zipWith distance sortedIds1 sortedIds2
  where
    distance x y = abs $ x - y

similarityScore :: Locations -> Locations -> Int
similarityScore (Locations sortedIds1) (Locations sortedIds2) =
  go (group sortedIds1) (group sortedIds2)
  where
    go [] _ = 0
    go _ [] = 0
    go all1@(group1:groups1) all2@(group2:groups2) =
      case head group1 `compare` head group2 of
        EQ -> length group1 * length group2 * head group1 + go groups1 groups2
        LT -> go groups1 all2
        GT -> go all1 groups2
