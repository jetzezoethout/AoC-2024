module Computer where

import           Control.Monad.Reader (ReaderT (runReaderT), asks, unless)
import           Control.Monad.State  (State, evalState, gets, modify)
import           Data.Bits            (xor)
import           Data.IntMap          (IntMap, (!?))
import qualified Data.IntMap          as M
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Parsers              (parseUnsignedInt)

data Computer = Computer
  { pointer   :: Int
  , registerA :: Int
  , registerB :: Int
  , registerC :: Int
  } deriving (Show)

parseComputer :: [Text] -> Computer
parseComputer textLines =
  Computer
    { pointer = 0
    , registerA = getRegisterValue $ head textLines
    , registerB = getRegisterValue $ textLines !! 1
    , registerC = getRegisterValue $ textLines !! 2
    }
  where
    getRegisterValue = parseUnsignedInt . (!! 1) . T.splitOn ": "

setPointer :: Int -> Computer -> Computer
setPointer newPointer computer = computer {pointer = newPointer}

setA :: Int -> Computer -> Computer
setA newA computer = computer {registerA = newA}

setB :: Int -> Computer -> Computer
setB newB computer = computer {registerB = newB}

setC :: Int -> Computer -> Computer
setC newC computer = computer {registerC = newC}

type Program = IntMap Int

parseProgram :: Text -> Program
parseProgram =
  M.fromList
    . zip [0 ..]
    . map parseUnsignedInt
    . T.splitOn ","
    . (!! 1)
    . T.splitOn ": "

type Runtime = ReaderT Program (State Computer)

scanPointer :: Runtime (Maybe Int)
scanPointer = do
  pointer <- gets pointer
  value <- asks (!? pointer)
  modify $ setPointer (pointer + 1)
  return value

data Operand
  = Literal Int
  | Combo Int

readOperand :: Operand -> Runtime Int
readOperand (Literal val) = return val
readOperand (Combo 4)     = gets registerA
readOperand (Combo 5)     = gets registerB
readOperand (Combo 6)     = gets registerC
readOperand (Combo val)   = return val

data Instruction
  = Adv Operand
  | Bxl Operand
  | Bst Operand
  | Jnz Operand
  | Bxc
  | Out Operand
  | Bdv Operand
  | Cdv Operand

readInstruction :: Runtime (Maybe Instruction)
readInstruction = do
  pointer <- scanPointer
  case pointer of
    Nothing -> return Nothing
    Just opcode -> do
      operandVal <- fromJust <$> scanPointer
      return
        $ Just
        $ case opcode of
            0 -> Adv $ Combo operandVal
            1 -> Bxl $ Literal operandVal
            2 -> Bst $ Combo operandVal
            3 -> Jnz $ Literal operandVal
            4 -> Bxc
            5 -> Out $ Combo operandVal
            6 -> Bdv $ Combo operandVal
            7 -> Cdv $ Combo operandVal
            _ -> error $ "invalid opcode " <> show opcode

execute :: Runtime [Int]
execute = go
  where
    go = do
      instruction <- readInstruction
      case instruction of
        Nothing -> return []
        Just (Adv operand) -> do
          num <- gets registerA
          denom <- readOperand operand
          modify $ setA (num `div` 2 ^ denom)
          go
        Just (Bxl operand) -> do
          arg1 <- gets registerB
          arg2 <- readOperand operand
          modify $ setB (arg1 `xor` arg2)
          go
        Just (Bst operand) -> do
          arg <- readOperand operand
          modify $ setB $ arg `mod` 8
          go
        Just (Jnz operand) -> do
          test <- gets registerA
          unless (test == 0) $ do
            jumpTo <- readOperand operand
            modify $ setPointer jumpTo
          go
        Just Bxc -> do
          arg1 <- gets registerB
          arg2 <- gets registerC
          modify $ setB $ arg1 `xor` arg2
          go
        Just (Out operand) -> do
          out <- readOperand operand
          (out `mod` 8 :) <$> go
        Just (Bdv operand) -> do
          num <- gets registerA
          denom <- readOperand operand
          modify $ setB (num `div` 2 ^ denom)
          go
        Just (Cdv operand) -> do
          num <- gets registerA
          denom <- readOperand operand
          modify $ setC (num `div` 2 ^ denom)
          go

run :: Program -> Computer -> [Int]
run program = evalState (runReaderT execute program)
