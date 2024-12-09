module Free where

class Free a where
  prepend :: Int -> Int -> a -> a
  request :: Int -> Int -> a -> ([Int], a)
