module Area where

import           Coordinate (Coordinate)
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

walk :: Area -> Guard -> Int
walk area = go S.empty
  where
    go seen guard =
      let newSeen = guard `S.insert` seen
       in case area `inspectAt` inFrontOf guard of
            Just Free     -> go newSeen $ forward guard
            Just Obstacle -> go newSeen $ turn guard
            Nothing       -> S.size $ S.map position newSeen

endsInLoop :: Area -> S.Set Guard -> Guard -> Bool
endsInLoop area seen guard =
  (guard `S.member` seen)
    || (let newSeen = guard `S.insert` seen
         in case area `inspectAt` inFrontOf guard of
              Just Free     -> endsInLoop area newSeen $ forward guard
              Just Obstacle -> endsInLoop area newSeen $ turn guard
              Nothing       -> False)

foolGuard :: Area -> Guard -> Int
foolGuard area = go S.empty S.empty
  where
    go tried seen guard =
      case area `inspectAt` inFrontOf guard of
        Just Free ->
          let extra =
                if not (inFrontOf guard `S.member` tried)
                     && endsInLoop (obstruct (inFrontOf guard) area) seen guard
                  then 1
                  else 0
           in extra
                + go
                    (inFrontOf guard `S.insert` tried)
                    (guard `S.insert` seen)
                    (forward guard)
        Just Obstacle -> go tried (guard `S.insert` seen) $ turn guard
        Nothing -> 0
