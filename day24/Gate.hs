module Gate where

import           Data.Bits (Bits (..))
import           Data.Text (Text)

data Gate
  = And
  | Or
  | Xor
  deriving (Show, Eq)

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
