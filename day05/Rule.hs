module Rule where

import           Data.Set  (Set)
import qualified Data.Set  as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Rule = Rule
  { before :: Int
  , after  :: Int
  } deriving (Eq, Ord)

parseRule :: Text -> Rule
parseRule text =
  let parts = T.splitOn "|" text
   in Rule
        { before = parseUnsignedInt $ head parts
        , after = parseUnsignedInt $ parts !! 1
        }

type Rules = Set Rule

parseRules :: [Text] -> Rules
parseRules = S.fromList . map parseRule

compareBy :: Rules -> Int -> Int -> Ordering
compareBy rules x y
  | Rule {before = x, after = y} `S.member` rules = LT
  | otherwise = GT
