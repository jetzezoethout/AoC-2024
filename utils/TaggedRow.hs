module TaggedRow where

import           Data.Text (Text)
import qualified Data.Text as T

data TaggedRow = TaggedRow
  { rowIndex :: Int
  , content  :: Text
  } deriving (Show)

parseTaggedLines :: Text -> [TaggedRow]
parseTaggedLines = zipLines . T.lines

zipLines :: [Text] -> [TaggedRow]
zipLines = zipWith TaggedRow [0 ..]
