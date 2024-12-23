module Main where

import           Data.Function (on)
import           Data.List     (maximumBy)
import qualified Data.Set      as S
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           LanParty      (findMaximalCliques, parseLanParty, triangles)
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let lanParty = parseLanParty text
    print $ S.size $ triangles lanParty
    TIO.putStrLn
      $ T.intercalate ","
      $ S.toAscList
      $ maximumBy (compare `on` S.size)
      $ findMaximalCliques lanParty
