module Main where

import Data.IntMap qualified as IMap
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

data Op
  = Skip
  | Add Int
  deriving (Generic, Show)

type Instruction = [Op]

add :: Int -> Instruction
add n = [Skip, Add n]

noop :: Instruction
noop = [Skip]

runOp :: Op -> Int -> Int
runOp Skip = id
runOp (Add n) = (+ n)

parseProgram :: Parsec Void Text [Instruction]
parseProgram = many $ do
  instruction <- parseNoop <|> parseAdd
  void newline
  return instruction

parseNoop :: Parsec Void Text Instruction
parseNoop = do
  void $ string "noop"
  return noop

parseAdd :: Parsec Void Text Instruction
parseAdd = do
  void $ string "addx "
  add <$> signed space decimal

newtype ProgramState = ProgramState
  { x :: Int
  }
  deriving (Generic, Show)

runProgram :: [Instruction] -> State ProgramState (IntMap Int)
runProgram program = do
  let opQueue = concat program
  cycleXVals <- forM (zip [1 ..] opQueue) $ \(cycleIdx, op) -> do
    xVal <- use #x
    #x %= runOp op
    return (cycleIdx, xVal)
  return $ IMap.fromList cycleXVals

initState :: ProgramState
initState = ProgramState {x = 1}

signalStrengthSum :: [Instruction] -> [Int] -> Int
signalStrengthSum program cycleIndices =
  let cycleVals = evalState (runProgram program) initState
   in sum $ fmap (\i -> i * cycleVals IMap.! i) cycleIndices

renderProgramOut :: Int -> IntMap Int -> String
renderProgramOut width cycleXVals =
  let allPixels =
        [ let i' = (i - 1) `mod` width
           in if x - 1 <= i' && i' <= x + 1 then '#' else '.'
          | (i, x) <- IMap.assocs cycleXVals
        ]
   in unlines $ chunksOf width allPixels

main :: IO ()
main = do
  testProgram <- parseOrDie parseProgram <$> TIO.readFile "aoc22_day10_test"
  let cycleIndices = [20, 60, 100, 140, 180, 220]
      actualSignalStrengthSum = signalStrengthSum testProgram cycleIndices
  assert (actualSignalStrengthSum == 13140) $ do
    program <- parseOrDie parseProgram <$> TIO.readFile "aoc22_day10_input"
    putStrLn $ "Question 1 answer is: " ++ show (signalStrengthSum program cycleIndices)
    let programOut = evalState (runProgram program) initState
    putStrLn "Question 2 answer is: "
    putStrLn $ renderProgramOut 40 programOut
