module Wire where

import           Data.Bits       (xor, (.&.), (.|.))
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map        as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers         (parseInt)

type WireLabel = Text

data Gate
  = And
  | Or
  | Xor
  deriving (Show)

parseGate :: Text -> Gate
parseGate "AND" = And
parseGate "OR"  = Or
parseGate "XOR" = Xor
parseGate _     = error "invalid connector"

type Signal = Int

gateEffect :: Gate -> Signal -> Signal -> Signal
gateEffect And = (.&.)
gateEffect Or  = (.|.)
gateEffect Xor = xor

data Wire
  = Source
      { signal :: Signal
      }
  | Connected
      { gate       :: Gate
      , leftInput  :: WireLabel
      , rightInput :: WireLabel
      }
  deriving (Show)

parseConnected :: Text -> Wire
parseConnected text =
  let parts = T.words text
   in Connected
        { gate = parseGate $ parts !! 1
        , leftInput = head parts
        , rightInput = parts !! 2
        }

type WireMap = Map WireLabel Wire

parseWireMap :: Text -> WireMap
parseWireMap text =
  M.fromList
    $ map parseSourceLine (head sections)
        <> map parseConnectedLine (sections !! 1)
  where
    sections = splitOn [""] $ T.lines text
    parseSourceLine textLine =
      let parts = T.splitOn ": " textLine
       in (head parts, Source $ parseInt $ parts !! 1)
    parseConnectedLine textLine =
      let parts = T.splitOn " -> " textLine
       in (parts !! 1, parseConnected $ head parts)

zWires :: WireMap -> [WireLabel]
zWires wireMap =
  sortBy (flip compare) $ filter ((== 'z') . T.head) $ M.keys wireMap

evaluate :: WireMap -> WireLabel -> Int
evaluate wireMap = go
  where
    go wireLabel =
      case wireMap ! wireLabel of
        Source {..}    -> signal
        Connected {..} -> gateEffect gate (go leftInput) (go rightInput)
