module Main where

import qualified Data.Text    as T
import           Parsers      (parseUnsignedInt)
import           ProcessFile  (processFile)
import           SecretNumber (generate)

main :: IO ()
main =
  processFile $ \text -> do
    let secretNumbers = map parseUnsignedInt $ T.lines text
    print $ sum $ map ((!! 2000) . generate) secretNumbers
