module ProcessFile
  ( processFile
  ) where

import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import           Numeric            (showFFloat)
import           System.Clock       (Clock (Realtime), diffTimeSpec, getTime,
                                     toNanoSecs)
import           System.Environment (getArgs)
import           System.IO          (IOMode (ReadMode), hClose, openFile)

processFile :: (Text -> IO ()) -> IO ()
processFile action = do
  t0 <- getTime Realtime
  fileName <- head <$> getArgs
  handle <- openFile fileName ReadMode
  contents <- TIO.hGetContents handle
  action contents
  hClose handle
  t1 <- getTime Realtime
  let time :: Double
      time = fromIntegral (toNanoSecs (diffTimeSpec t1 t0)) / 1000000000
  putStrLn $ "Time: " <> showFFloat (Just 3) time "s"
