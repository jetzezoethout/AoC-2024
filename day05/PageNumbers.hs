module PageNumbers where

import           Data.List (sortBy)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)
import           Rule      (Rules, compareBy)

type PageNumbers = [Int]

parsePageNumbers :: Text -> [Int]
parsePageNumbers = map parseUnsignedInt . T.splitOn ","

middle :: PageNumbers -> Int
middle pageNumbers = pageNumbers !! (length pageNumbers `div` 2)

isInOrder :: Rules -> PageNumbers -> Bool
isInOrder rules pageNumbers =
  and $ zipWith areInOrder pageNumbers $ tail pageNumbers
  where
    areInOrder x y = compareBy rules x y == LT

order :: Rules -> PageNumbers -> PageNumbers
order = sortBy . compareBy
