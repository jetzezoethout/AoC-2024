module Area where

import           Coordinate    (Coordinate)
import           Data.Function (on)
import           Data.List     (nubBy)
import           Data.Text     (Text)
import           Grid          (Grid, parseGrid, safeAtCoordinate)
import           Guard         (Guard (..), forward, inFrontOf, turn)

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

walk :: Area -> Guard -> [Guard]
walk area = go
  where
    go :: Guard -> [Guard]
    go guard =
      guard
        : case area `inspectAt` inFrontOf guard of
            Just Free     -> go (forward guard)
            Just Obstacle -> go (turn guard)
            Nothing       -> []

visited :: Area -> Guard -> [Coordinate]
visited area = map position . nubBy ((==) `on` position) . walk area

isSuitableObstacle :: Area -> Guard -> Coordinate -> Bool
isSuitableObstacle area guard obstacle =
  isLoop $ walk (obstruct obstacle area) guard

-- Floyd's tortoise and hare loop detection algo
isLoop :: Eq a => [a] -> Bool
isLoop stream = go stream stream
  where
    go (x:xs) (_:y:ys) = x == y || go xs ys
    go _ _             = False
