module Stones where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List   (foldl')
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)

splitNumber :: Int -> Maybe (Int, Int)
splitNumber number =
  [ let (s1, s2) = splitAt (numberLength `div` 2) numberString
   in (read s1, read s2)
  | even numberLength
  ]
  where
    numberString = show number
    numberLength = length numberString

evolve :: Int -> [Int]
evolve stone =
  case splitNumber stone of
    Just (n1, n2) -> [n1, n2]
    Nothing ->
      case stone of
        0       -> [1]
        nonZero -> [2024 * nonZero]

type Stones = IntMap Int

totalStones :: Stones -> Int
totalStones = sum . M.elems

parseStones :: Text -> Stones
parseStones = M.fromListWith (+) . map ((, 1) . parseUnsignedInt) . T.words

blinkOnce :: Stones -> Stones
blinkOnce stones = M.fromListWith (+) $ M.foldrWithKey processStone [] stones
  where
    processStone stone amount acc = map (, amount) (evolve stone) <> acc

blink :: Stones -> Int -> Stones
blink stones times = foldl' (flip ($)) stones $ replicate times blinkOnce
