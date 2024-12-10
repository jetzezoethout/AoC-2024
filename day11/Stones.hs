module Stones where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List   (foldl')
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)

splitNumber :: Int -> Maybe (Int, Int)
splitNumber n =
  let nString = show n
      nLength = length nString
   in if even nLength
        then let (s1, s2) = splitAt (nLength `div` 2) nString
              in Just (read s1, read s2)
        else Nothing

type Stones = IntMap Int

totalStones :: Stones -> Int
totalStones = sum . M.elems

parseStones :: Text -> Stones
parseStones =
  M.fromListWith (+) . map ((, 1) . parseUnsignedInt) . T.words . T.strip

blinkOnce :: Stones -> Stones
blinkOnce stones =
  M.fromListWith (+)
    $ M.foldrWithKey (\stone amount acc -> henk stone amount <> acc) [] stones
  where
    henk stone amount =
      map (, amount)
        $ case splitNumber stone of
            Just (n1, n2) -> [n1, n2]
            Nothing ->
              case stone of
                0       -> [1]
                nonZero -> [2024 * nonZero]

blink :: Stones -> Int -> Stones
blink stones times = foldl' (flip ($)) stones $ replicate times blinkOnce
