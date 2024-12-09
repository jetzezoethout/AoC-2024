module Fragmentable where

import           Data.Text (Text)
import           Disk      (Disk, parseDisk)
import           Free      (Free (..))

newtype Fragmentable = Fragmentable
  { emptyAddresses :: [Int]
  }

instance Free Fragmentable where
  prepend :: Int -> Int -> Fragmentable -> Fragmentable
  prepend loc size Fragmentable {..} =
    Fragmentable {emptyAddresses = [loc .. loc + size - 1] <> emptyAddresses}
  request :: Int -> Int -> Fragmentable -> ([Int], Fragmentable)
  request size upTo Fragmentable {..} =
    let (freeSpaces, remaining) = splitAt size emptyAddresses
        (inRange, outRange) = span (< upTo) freeSpaces
     in (inRange, Fragmentable {emptyAddresses = outRange <> remaining})

parseFragmentableDisc :: Text -> Disk Fragmentable
parseFragmentableDisc = parseDisk $ Fragmentable []
