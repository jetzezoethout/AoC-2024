module Report where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

type Report = [Int]

parseReport :: Text -> Report
parseReport = map parseUnsignedInt . T.words

isSafe :: Report -> Bool
isSafe report =
  case head report `compare` (report !! 1) of
    EQ -> False
    LT -> checkOnConsecutivePairs (\x y -> 0 < y - x && y - x <= 3) report
    GT -> checkOnConsecutivePairs (\x y -> 0 < x - y && x - y <= 3) report
  where
    checkOnConsecutivePairs :: (a -> a -> Bool) -> [a] -> Bool
    checkOnConsecutivePairs p xs = and $ zipWith p xs $ tail xs

isDampenerSafe :: Report -> Bool
isDampenerSafe report = any isSafe $ withOneRemoved report
  where
    withOneRemoved :: Report -> [Report]
    withOneRemoved []     = []
    withOneRemoved (x:xs) = xs : map (x :) (withOneRemoved xs)
