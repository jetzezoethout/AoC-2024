module Unfragmentable where

import           Data.Function    (on)
import           Data.IntMap      (IntMap)
import qualified Data.IntMap.Lazy as M
import           Data.IntSet      (IntSet)
import qualified Data.IntSet      as S
import           Data.List        (minimumBy)
import           Data.Text        (Text)
import           Disk             (Disk, parseDisk)
import           Free             (Free (..))

newtype Unfragmentable = Unfragmentable
  { bySize :: IntMap IntSet
  }

instance Free Unfragmentable where
  prepend :: Int -> Int -> Unfragmentable -> Unfragmentable
  prepend loc size Unfragmentable {..} =
    Unfragmentable {bySize = M.adjust (S.insert loc) size bySize}
  request :: Int -> Int -> Unfragmentable -> ([Int], Unfragmentable)
  request size upTo Unfragmentable {..} =
    let isViable gapSize loc = gapSize >= size && loc < upTo
        viableGaps =
          M.toList
            $ M.filterWithKey isViable
            $ M.map fst
            $ M.mapMaybe S.minView bySize
     in if null viableGaps
          then ([], Unfragmentable {bySize = bySize})
          else let (gapSize, loc) = minimumBy (compare `on` snd) viableGaps
                in ( [loc .. loc + size - 1]
                   , Unfragmentable
                       { bySize =
                           M.adjust (S.insert (loc + size)) (gapSize - size)
                             $ M.adjust (S.delete loc) gapSize bySize
                       })

parseUnfragmentableDisk :: Text -> Disk Unfragmentable
parseUnfragmentableDisk =
  parseDisk $ Unfragmentable $ M.fromList $ map (, S.empty) [0 .. 9]
