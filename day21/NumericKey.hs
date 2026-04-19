module NumericKey where

import           ArrowKey   (shortestHumanLength)
import           Coordinate (Coordinate (..))
import           Data.Char  (digitToInt)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           KeyPad     (KeyPad (..))

data NumericKey
  = NumericKey Int
  | ApplyKey

instance Show NumericKey where
  show :: NumericKey -> String
  show (NumericKey num) = show num
  show ApplyKey         = "A"

parseNumericalKey :: Char -> NumericKey
parseNumericalKey 'A' = ApplyKey
parseNumericalKey ch  = NumericKey $ digitToInt ch

parseSequence :: Text -> [NumericKey]
parseSequence = map parseNumericalKey . T.unpack

numericPart :: [NumericKey] -> Int
numericPart keys = read $ init keys >>= show

instance KeyPad NumericKey where
  location :: NumericKey -> Coordinate
  location ApplyKey       = Coordinate 3 2
  location (NumericKey 0) = Coordinate 3 1
  location (NumericKey n) = Coordinate ((9 - n) `div` 3) ((n - 1) `mod` 3)
  atLocation :: Coordinate -> Maybe NumericKey
  atLocation Coordinate {..}
    | 0 <= row && row <= 2 && 0 <= column && column <= 2 =
      Just $ NumericKey $ 3 * (2 - row) + column + 1
    | row == 3 && column == 1 = Just $ NumericKey 0
    | row == 3 && column == 2 = Just ApplyKey
    | otherwise = Nothing
  applyKey :: NumericKey
  applyKey = ApplyKey

complexity :: Int -> [NumericKey] -> Int
complexity numArrowPads keys =
  shortestHumanLength numArrowPads keys * numericPart keys
