module LocatedChar where

import           Control.Monad (join)
import           Coordinate    (Coordinate (..))
import           Data.Text     (Text)
import qualified Data.Text     as T

data LocatedChar = LocatedChar
  { location :: Coordinate
  , char     :: Char
  }

buildLocatedRow :: Int -> Int -> Char -> LocatedChar
buildLocatedRow row column char =
  LocatedChar {location = Coordinate {row = row, column = column}, char = char}

locateText :: Text -> [LocatedChar]
locateText = locateTextLines . T.lines

locateTextLines :: [Text] -> [LocatedChar]
locateTextLines = join . zipWith locateRow [0 ..]
  where
    locateRow :: Int -> Text -> [LocatedChar]
    locateRow row = zipWith (buildLocatedRow row) [0 ..] . T.unpack
