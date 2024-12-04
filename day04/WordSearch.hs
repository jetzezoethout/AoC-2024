module WordSearch where

import           Coordinate (Coordinate)
import           Data.Maybe (mapMaybe)
import           Data.Text  (Text)
import           Direction  (Direction (..), allDirections, moveTowards)
import           Grid       (Grid, allCoordinates, parseGrid, safeAtCoordinate)
import           Token      (Token (..), fromChar, next)

type WordSearch = Grid Token

parseWordSearch :: Text -> WordSearch
parseWordSearch = parseGrid fromChar

northWest :: Coordinate -> Coordinate
northWest = (`moveTowards` West) . (`moveTowards` North)

northEast :: Coordinate -> Coordinate
northEast = (`moveTowards` East) . (`moveTowards` North)

southWest :: Coordinate -> Coordinate
southWest = (`moveTowards` West) . (`moveTowards` South)

southEast :: Coordinate -> Coordinate
southEast = (`moveTowards` East) . (`moveTowards` South)

type Move = Coordinate -> Coordinate

moves :: [Move]
moves =
  [(`moveTowards` dir) | dir <- allDirections]
    <> [northWest, northEast, southWest, southEast]

findXmas :: WordSearch -> Int
findXmas wordSearch =
  sum $ map (findXmasAt wordSearch) $ allCoordinates wordSearch

findXmasAt :: WordSearch -> Coordinate -> Int
findXmasAt wordSearch position =
  length $ filter (findTowards wordSearch position) moves

findTowards :: WordSearch -> Coordinate -> Move -> Bool
findTowards wordSearch position move = go position (Just X)
  where
    go currentPosition toFind =
      case toFind of
        Just token ->
          wordSearch `safeAtCoordinate` currentPosition == Just token
            && go (move currentPosition) (next token)
        Nothing -> True

findCrossMas :: WordSearch -> Int
findCrossMas wordSearch =
  length $ filter (findCrossMasAt wordSearch) $ allCoordinates wordSearch

findCrossMasAt :: WordSearch -> Coordinate -> Bool
findCrossMasAt wordSearch position = diagonalMas && antiDiagonalMas
  where
    diagonalMas =
      isMas
        $ mapMaybe
            (wordSearch `safeAtCoordinate`)
            [northWest position, position, southEast position]
    antiDiagonalMas =
      isMas
        $ mapMaybe
            (wordSearch `safeAtCoordinate`)
            [northEast position, position, southWest position]

isMas :: [Token] -> Bool
isMas tokenRow = tokenRow `elem` [[M, A, S], [S, A, M]]
