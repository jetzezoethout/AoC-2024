module Equation where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (mapMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Parsers            (parseUnsignedInt)

data Equation = Equation
  { testValue :: Int
  , numbers   :: NonEmpty Int
  } deriving (Show)

parseEquation :: Text -> Equation
parseEquation text =
  let parts = T.splitOn ": " text
   in Equation
        { testValue = parseUnsignedInt $ head parts
        , numbers = NE.fromList $ map parseUnsignedInt $ T.words $ parts !! 1
        }

type InverseOperation = Int -> Int -> Maybe Int

applyTo :: Int -> Int -> InverseOperation -> Maybe Int
applyTo target x f = f target x

unPlus :: InverseOperation
unPlus target x = [target - x | target > x]

unTimes :: InverseOperation
unTimes target x = [target `div` x | target `mod` x == 0]

unConcatenate :: InverseOperation
unConcatenate target x =
  [ parseUnsignedInt prefix
  | prefix <- T.stripSuffix (toText x) (toText target)
  , T.length prefix > 0
  ]
  where
    toText = T.pack . show

canBeTrueWith :: Equation -> [InverseOperation] -> Bool
Equation {..} `canBeTrueWith` unOperations =
  case NE.reverse numbers of
    lastNumber :| initNumbers -> go lastNumber initNumbers testValue
  where
    go current [] target = target == current
    go current (next:remaining) target =
      any (go next remaining) $ mapMaybe (applyTo target current) unOperations

canBeTrue :: Equation -> Bool
canBeTrue = (`canBeTrueWith` [unPlus, unTimes])

canBeTrueWithConcatenation :: Equation -> Bool
canBeTrueWithConcatenation = (`canBeTrueWith` [unPlus, unTimes, unConcatenate])
