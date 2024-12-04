module Token where

data Token
  = X
  | M
  | A
  | S
  deriving (Eq, Show, Enum)

fromChar :: Char -> Token
fromChar 'X' = X
fromChar 'M' = M
fromChar 'A' = A
fromChar 'S' = S
fromChar _   = error "unknown token"

next :: Token -> Maybe Token
next token =
  if fromEnum token < 3
    then Just $ toEnum $ fromEnum token + 1
    else Nothing
