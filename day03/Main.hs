module Main where

import           Instruction       (evaluate, evaluateConditional)
import           InstructionParser (collectInstructions)
import           ProcessFile       (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let mulInstructions = collectInstructions text
    print $ evaluate mulInstructions
    print $ evaluateConditional mulInstructions
