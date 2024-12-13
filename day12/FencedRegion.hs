module FencedRegion where

import           Coordinate (Coordinate)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Direction  (Direction, allDirections, clockWise, moveTowards)
import           Region     (Region)

data Edge = Edge
  { at      :: Coordinate
  , towards :: Direction
  } deriving (Eq, Ord)

shiftClockWise :: Edge -> Edge
shiftClockWise Edge {..} =
  Edge {at = at `moveTowards` clockWise towards, towards = towards}

type Fence = Set Edge

data FencedRegion = FencedRegion
  { region :: Region
  , fence  :: Fence
  }

putFence :: Region -> FencedRegion
putFence region = FencedRegion {..}
  where
    fence = S.foldl' addFence S.empty region
    addFence acc plot =
      foldr
        (S.insert . Edge plot)
        acc
        (filter ((`S.notMember` region) . (plot `moveTowards`)) allDirections)

price :: FencedRegion -> Int
price FencedRegion {..} = S.size region * S.size fence

discountedPrice :: FencedRegion -> Int
discountedPrice FencedRegion {..} = S.size region * sides
  where
    sides = S.size $ S.filter ((`S.notMember` fence) . shiftClockWise) fence
