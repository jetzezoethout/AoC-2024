module Antennas where

import           Coordinate  (Coordinate, addCoordinate, dilate)
import           Data.List   (nub)
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (mapMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Dimensions  (Dimensions, isInBounds, parseDimensions)
import           LocatedChar (LocatedChar (..), locateTextLines)

type Frequency = Char

data Antennas = Antennas
  { dimensions  :: Dimensions
  , byFrequency :: Map Frequency [Coordinate]
  }

parseAntennas :: Text -> Antennas
parseAntennas text =
  Antennas
    { dimensions = parseDimensions textLines
    , byFrequency =
        M.fromListWith (<>) $ mapMaybe getAntenna $ locateTextLines textLines
    }
  where
    textLines = T.lines text
    getAntenna LocatedChar {..} = [(char, [location]) | char /= '.']

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x, ) xs <> map (, x) xs <> pairs xs

type AntinodeGenerator = Dimensions -> Coordinate -> Coordinate -> [Coordinate]

step :: Coordinate -> Coordinate -> Int -> Coordinate
step from reference distance =
  ((distance + 1) `dilate` from)
    `addCoordinate` ((-distance) `dilate` reference)

singleAntinode :: AntinodeGenerator
singleAntinode dimensions from reference =
  filter (isInBounds dimensions) [step from reference 1]

harmonicAntinodes :: AntinodeGenerator
harmonicAntinodes dimensions from reference =
  takeWhile (isInBounds dimensions) $ (map $ step from reference) [0 ..]

antinodesFor :: AntinodeGenerator -> Antennas -> [Coordinate]
antinodesFor generator Antennas {..} =
  nub $ do
    antennaGroup <- M.elems byFrequency
    (from, reference) <- pairs antennaGroup
    generator dimensions from reference

getAntinodes :: Antennas -> [Coordinate]
getAntinodes = antinodesFor singleAntinode

getHarmonicAntinodes :: Antennas -> [Coordinate]
getHarmonicAntinodes = antinodesFor harmonicAntinodes
