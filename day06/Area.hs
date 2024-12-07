module Area where

import           Coordinate (Coordinate)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Data.Text  (Text)
import           Grid       (Grid, parseGrid, safeAtCoordinate)
import           Guard      (Guard (..), forward, inFrontOf, turn)

data AreaElement
  = Obstacle
  | Free
  deriving (Eq)

fromChar :: Char -> AreaElement
fromChar '#' = Obstacle
fromChar _   = Free

data Area = Area
  { original :: Grid AreaElement
  , added    :: Maybe Coordinate
  }

parseArea :: Text -> Area
parseArea text = Area {original = parseGrid fromChar text, added = Nothing}

inspectAt :: Area -> Coordinate -> Maybe AreaElement
Area {..} `inspectAt` coord =
  if added == Just coord
    then Just Obstacle
    else original `safeAtCoordinate` coord

obstruct :: Coordinate -> Area -> Area
obstruct obstacle area = area {added = Just obstacle}

visitedLocations :: Area -> Guard -> Int
visitedLocations area = go S.empty
  where
    go visited guard =
      let updatedVisited = position guard `S.insert` visited
       in case area `inspectAt` inFrontOf guard of
            Just Free     -> go updatedVisited $ forward guard
            Just Obstacle -> go updatedVisited $ turn guard
            Nothing       -> S.size updatedVisited

endsInLoop :: Area -> Set Guard -> Guard -> Bool
endsInLoop area = go
  where
    go previousBlocks guard =
      case area `inspectAt` inFrontOf guard of
        Just Free -> go previousBlocks $ forward guard
        Just Obstacle ->
          (guard `S.member` previousBlocks)
            || go (guard `S.insert` previousBlocks) (turn guard)
        Nothing -> False

foolGuard :: Area -> Guard -> Int
foolGuard area start = go 0 (S.singleton $ position start) S.empty start
  where
    go acc tried previousBlocks guard =
      case area `inspectAt` inFrontOf guard of
        Just Free ->
          let toObstruct = inFrontOf guard
              isSuccess =
                not (toObstruct `S.member` tried)
                  && endsInLoop (obstruct toObstruct area) previousBlocks guard
           in go
                (if isSuccess
                   then acc + 1
                   else acc)
                (toObstruct `S.insert` tried)
                previousBlocks
                (forward guard)
        Just Obstacle ->
          go acc tried (guard `S.insert` previousBlocks) $ turn guard
        Nothing -> acc
