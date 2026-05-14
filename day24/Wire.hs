module Wire where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromJust)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Gate       (Signal)
import           Parsers    (parseInt)

type WireLabel = Text

data SourceTag
  = X
  | Y
  deriving (Show, Eq, Ord)

fromChar :: Char -> Maybe SourceTag
fromChar 'x' = Just X
fromChar 'y' = Just Y
fromChar _   = Nothing

data InputWire = InputWire
  { sourceTag  :: SourceTag
  , inputIndex :: Int
  } deriving (Show, Eq, Ord)

toInputWire :: WireLabel -> Maybe InputWire
toInputWire text = do
  let (ch, chs) = fromJust $ T.uncons text
  sourceTag <- fromChar ch
  let inputIndex = parseInt chs
  return InputWire {..}

parseInputWireSignals :: [Text] -> Map InputWire Signal
parseInputWireSignals = M.fromList . map parseLine
  where
    parseLine :: Text -> (InputWire, Signal)
    parseLine textLine =
      let parts = T.splitOn ": " textLine
       in (fromJust $ toInputWire $ head parts, parseInt $ parts !! 1)

newtype OutputWire = OutputWire
  { outputIndex :: Int
  } deriving (Show)

outputWireLabel :: OutputWire -> WireLabel
outputWireLabel OutputWire {..} =
  "z" <> T.justifyRight 2 '0' (T.pack $ show outputIndex)

toOutputWire :: WireLabel -> Maybe OutputWire
toOutputWire text =
  let (ch, chs) = fromJust $ T.uncons text
   in [OutputWire {outputIndex = parseInt chs} | ch == 'z']
