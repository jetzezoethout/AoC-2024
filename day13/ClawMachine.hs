module ClawMachine where

import           Coordinate (Coordinate (..), addCoordinate, determinant)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseUnsignedInt)

data ClawMachine = ClawMachine
  { aMovement :: Coordinate
  , bMovement :: Coordinate
  , target    :: Coordinate
  } deriving (Show)

parseCoordinateAfter :: Text -> Text -> Coordinate
parseCoordinateAfter precursor text =
  let parts = T.splitOn precursor text
   in Coordinate
        { row = parseUnsignedInt $ parts !! 2
        , column = parseUnsignedInt $ parts !! 1
        }

parseClawMachine :: [Text] -> ClawMachine
parseClawMachine textLines =
  ClawMachine
    { aMovement = parseCoordinateAfter "+" $ head textLines
    , bMovement = parseCoordinateAfter "+" $ textLines !! 1
    , target = parseCoordinateAfter "=" $ textLines !! 2
    }

calculateA :: ClawMachine -> Maybe Int
calculateA ClawMachine {..} =
  let mainDet = determinant aMovement bMovement
      specificDet = determinant target bMovement
   in [specificDet `div` mainDet | specificDet `mod` mainDet == 0]

calculateB :: ClawMachine -> Maybe Int
calculateB ClawMachine {..} =
  let mainDet = determinant aMovement bMovement
      specificDet = determinant aMovement target
   in [specificDet `div` mainDet | specificDet `mod` mainDet == 0]

tokensRequired :: ClawMachine -> Maybe Int
tokensRequired machine = do
  pressesA <- calculateA machine
  pressesB <- calculateB machine
  return $ 3 * pressesA + pressesB

blowUp :: ClawMachine -> ClawMachine
blowUp machine = machine {target = machine.target `addCoordinate` correction}
  where
    correction = Coordinate {row = 10000000000000, column = 10000000000000}
