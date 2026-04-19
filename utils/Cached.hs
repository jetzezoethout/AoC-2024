module Cached where

import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M

type Cached k v = State (Map k v) v

cacheUnder :: (Ord k) => k -> v -> Cached k v
cacheUnder key value = do
  modify $ M.insert key value
  return value

withCache :: (Ord k) => (k -> Cached k v) -> k -> Cached k v
withCache actualComputation key = do
  optionalResult <- gets $ M.lookup key
  maybe (actualComputation key >>= cacheUnder key) return optionalResult

runMemoized :: Cached k v -> v
runMemoized cached = evalState cached M.empty
