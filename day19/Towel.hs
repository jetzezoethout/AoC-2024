module Towel where

import           Cached     (Cached, runMemoized, withCache)
import           Data.Maybe (mapMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T

type Towel = Text

type Pattern = Text

parseTowels :: Text -> [Towel]
parseTowels = T.splitOn ", "

makeWith :: [Towel] -> Pattern -> Int
makeWith towels = runMemoized . go
  where
    go :: Text -> Cached Text Int
    go =
      withCache $ \case
        "" -> return 1
        nonEmpty -> do
          makeSubpatterns <-
            traverse go $ mapMaybe (`T.stripPrefix` nonEmpty) towels
          return $ sum makeSubpatterns
