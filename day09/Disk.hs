module Disk where

import           Data.Char   (digitToInt)
import           Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Free        (Free (prepend, request))

data Disk a = Disk
  { files :: IntMap [Int]
  , free  :: a
  } deriving (Show)

emptyDisk :: a -> Disk a
emptyDisk emptyFree = Disk {files = M.empty, free = emptyFree}

withFile :: Int -> [Int] -> Disk a -> Disk a
withFile fileId space Disk {..} =
  Disk {files = M.insert fileId space files, free = free}

preprendFree :: Free a => Int -> Int -> Disk a -> Disk a
preprendFree loc size Disk {..} =
  Disk {files = files, free = prepend loc size free}

parseDisk :: Free a => a -> Text -> Disk a
parseDisk emptyFree text = fromFile 0 0 numbers
  where
    numbers = map digitToInt $ T.unpack $ T.strip text
    fromFile _ _ [] = emptyDisk emptyFree
    fromFile loc fileId (size:remaining) =
      withFile fileId [loc .. loc + size - 1]
        $ fromFree (loc + size) (fileId + 1) remaining
    fromFree _ _ [] = emptyDisk emptyFree
    fromFree loc fileId (size:remaining) =
      preprendFree loc size $ fromFile (loc + size) fileId remaining

lastFileId :: Disk a -> Int
lastFileId Disk {..} = M.size files - 1

compactify :: Free a => Disk a -> Disk a
compactify disk = go (lastFileId disk) disk
  where
    go toMove Disk {..} =
      let source = files ! toMove
          (dest, newFree) = request (length source) (head source) free
          sizeLeft = length source - length dest
          updatedDisk2 =
            Disk
              { files = M.insert toMove (dest <> take sizeLeft source) files
              , free = newFree
              }
       in if toMove > 0
            then go (toMove - 1) updatedDisk2
            else updatedDisk2

checkSum :: Disk a -> Int
checkSum Disk {..} = M.foldlWithKey' folder 0 files
  where
    folder acc fileId memory = acc + fileId * sum memory
