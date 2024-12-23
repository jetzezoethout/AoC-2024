module LanParty where

import           Data.List (foldl')
import           Data.Map  (Map, (!))
import qualified Data.Map  as M
import           Data.Set  (Set)
import qualified Data.Set  as S
import           Data.Text (Text)
import qualified Data.Text as T

type ComputerName = Text

type LanParty = Map ComputerName (Set ComputerName)

parseLanParty :: Text -> LanParty
parseLanParty = foldl' processLine M.empty . T.lines
  where
    processLine acc text =
      let parts = T.splitOn "-" text
          computer1 = head parts
          computer2 = parts !! 1
       in addConnection computer1 computer2
            $ addConnection computer2 computer1 acc
    addConnection source target =
      M.insertWith S.union source $ S.singleton target

areConnected :: LanParty -> ComputerName -> ComputerName -> Bool
areConnected lanParty computer1 computer2 =
  computer2 `S.member` (lanParty ! computer1)

computers :: LanParty -> Set ComputerName
computers = M.keysSet

pairs :: Set a -> [(a, a)]
pairs = go . S.toList
  where
    go []     = []
    go (x:xs) = map (x, ) xs <> go xs

type Clique = Set ComputerName

triangles :: LanParty -> Set Clique
triangles lanParty =
  S.foldl' S.union S.empty
    $ S.map trianglesFrom
    $ S.filter ("t" `T.isPrefixOf`)
    $ computers lanParty
  where
    trianglesFrom :: ComputerName -> Set Clique
    trianglesFrom computer =
      S.fromList
        [ S.fromList [computer, connected1, connected2]
        | (connected1, connected2) <- pairs $ lanParty ! computer
        , areConnected lanParty connected1 connected2
        ]

-- The Bron-Kerbosch algorithm for finding maximal cliques
findMaximalCliques :: LanParty -> [Clique]
findMaximalCliques lanParty = go S.empty (computers lanParty) S.empty
  where
    go :: Set ComputerName -> Set ComputerName -> Set ComputerName -> [Clique]
    go clique candidates forbidden =
      case S.minView candidates of
        Nothing -> [clique | S.size forbidden == 0]
        Just (vertex, remaining) ->
          go
            (S.insert vertex clique)
            (S.intersection candidates $ lanParty ! vertex)
            (S.intersection forbidden $ lanParty ! vertex)
            <> go clique remaining (S.insert vertex forbidden)
