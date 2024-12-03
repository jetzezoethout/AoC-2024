module InstructionParser where

import           Control.Applicative (Alternative (..))
import           Control.Monad.State (StateT (StateT), evalStateT, guard)
import           Data.Maybe          (catMaybes, fromJust)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Read      (decimal)
import           Instruction         (Instruction (..))

type Parser = StateT Text Maybe

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)      = Nothing
rightToMaybe (Right value) = Just value

readChar :: Parser Char
readChar = StateT T.uncons

dropChar :: Char -> Parser ()
dropChar ch = do
  actual <- readChar
  guard $ actual == ch

dropText :: Text -> Parser ()
dropText text = go $ T.unpack text
  where
    go ""       = return ()
    go (ch:chs) = dropChar ch >> go chs

readUnsignedInt :: Parser Int
readUnsignedInt = StateT $ rightToMaybe . decimal

readThreeDigitNumber :: Parser Int
readThreeDigitNumber = do
  number <- readUnsignedInt
  guard $ number < 1000
  return number

readDo :: Parser Instruction
readDo = dropText "do()" >> return Do

readDont :: Parser Instruction
readDont = dropText "don't()" >> return Dont

readMul :: Parser Instruction
readMul = do
  dropText "mul("
  arg1 <- readThreeDigitNumber
  dropText ","
  arg2 <- readThreeDigitNumber
  dropText ")"
  return Mul {..}

readInstruction :: Parser Instruction
readInstruction = readDo <|> readDont <|> readMul

collect :: Parser a -> Parser [a]
collect parser = catMaybes <$> many (collectOne <|> proceed)
  where
    collectOne = Just <$> parser
    proceed = readChar >> return Nothing

collectInstructions :: Text -> [Instruction]
collectInstructions = fromJust . evalStateT (collect readInstruction)
