module Connection where

import           Control.Monad (guard)
import           Data.List     (foldl')
import           Data.Maybe    (isJust, mapMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Gate          (Gate (..), Signal, gateEffect, parseGate)
import           Wire          (InputWire (..), OutputWire (..), WireLabel,
                                outputWireLabel, toInputWire, toOutputWire)

data Connection = Connection
  { in1    :: WireLabel
  , in2    :: WireLabel
  , gate   :: Gate
  , output :: WireLabel
  } deriving (Show)

parseConnections :: [Text] -> [Connection]
parseConnections = map parseConnection
  where
    parseConnection textLine =
      let parts = T.splitOn " -> " textLine
          inputParts = T.words $ head parts
       in Connection
            { in1 = head inputParts
            , in2 = inputParts !! 2
            , gate = parseGate $ inputParts !! 1
            , output = parts !! 1
            }

findByOutput :: [Connection] -> WireLabel -> Connection
findByOutput connections desiredOutput =
  head $ filter (\Connection {..} -> output == desiredOutput) connections

registerWidth :: [Connection] -> Int
registerWidth connections =
  maximum $ map outputIndex $ mapMaybe (toOutputWire . output) connections

evaluate :: [Connection] -> (InputWire -> Signal) -> Int
evaluate connections getInputSignal =
  fromBinary
    $ reverse
    $ map
        (getSignal . outputWireLabel . OutputWire)
        [0 .. registerWidth connections]
  where
    fromBinary :: [Int] -> Int
    fromBinary = foldl' (\x y -> 2 * x + y) 0
    getSignal :: WireLabel -> Signal
    getSignal label =
      case toInputWire label of
        Just inputWire -> getInputSignal inputWire
        Nothing ->
          let Connection {..} = findByOutput connections label
           in gateEffect gate (getSignal in1) (getSignal in2)

isGoodConnection :: [Connection] -> Connection -> Bool
isGoodConnection connections = test
  where
    test :: Connection -> Bool
    test Connection {..} =
      case gate of
        -- An XOR connection must be an input connection and followed by another XOR connection
        -- or be an output connection.
        Xor ->
          let isInputConnection = isJust $ toInputWire in1
              isOutputConnection = isJust (toOutputWire output)
           in isOutputConnection
                || (isInputConnection && output `isConnectedTo` Xor)
        -- An AND gate must be connected to an OR gate,
        -- except in the first half-adder (where it produces the first carry bit)
        And ->
          let isInFirstHalfAdder =
                isJust $ do
                  inputWire <- toInputWire in1
                  guard $ inputIndex inputWire == 0
           in isInFirstHalfAdder || output `isConnectedTo` Or
        -- An OR gate must be followed by an XOR gate,
        -- except in the last full adder, where the carry bit is output as z_max
        Or ->
          let isFinalOutput =
                output
                  == outputWireLabel (OutputWire $ registerWidth connections)
           in isFinalOutput || output `isConnectedTo` Xor
    isConnectedTo :: WireLabel -> Gate -> Bool
    wire `isConnectedTo` desiredGate =
      any
        (\Connection {..} -> gate == desiredGate && (in1 == wire || in2 == wire))
        connections

badWires :: [Connection] -> [WireLabel]
badWires connections =
  map output $ filter (not . isGoodConnection connections) connections
