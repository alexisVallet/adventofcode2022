module Main where

import Data.Sequence qualified as Seq
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Imports
import ParseUtils

data Monkey = Monkey
  { items :: Seq Int,
    operation :: Int -> Int,
    -- runs the test and outputs the index of the monkey
    -- to throw to.
    test :: Int -> Int,
    testDiv :: Int
  }
  deriving (Generic)

charToOp :: Char -> (Int -> Int -> Int)
charToOp '+' = (+)
charToOp '*' = (*)
charToOp c = error $ "unsupported operation " ++ show c

parseBinOp :: Parsec Void Text (Int -> Int)
parseBinOp = do
  binOp <- charToOp <$> oneOf ("+*" :: String)
  hspace
  cstOrOld <- Left <$> decimal <|> Right <$> string "old"
  return $ case cstOrOld of
    Left constant -> binOp constant
    Right _ -> \i -> binOp i i

parseMonkeys :: Parsec Void Text (Vector Monkey)
parseMonkeys = fmap V.fromList $
  many $ do
    -- Monkeys are in order from 0 to N - 1 so we
    -- don't bother parsing the first line.
    skipLines 1
    hspace
    void $ string "Starting items: "
    startingItems <- Seq.fromList <$> sepBy decimal ", "
    newline
    hspace
    void $ string "Operation: new = old "
    operation <- parseBinOp
    newline
    hspace
    void $ string "Test: divisible by "
    divByVal <- decimal
    newline
    hspace
    void $ string "If true: throw to monkey "
    trueTarget <- decimal
    newline
    hspace
    void $ string "If false: throw to monkey "
    falseTarget <- decimal
    space
    return $
      Monkey
        { items = startingItems,
          operation = operation,
          test = \i -> if (i `mod` divByVal) == 0 then trueTarget else falseTarget,
          testDiv = divByVal
        }

data KeepAwayState = KeepAwayState
  { monkeys :: Vector Monkey,
    inspectionCounts :: Vector Int,
    itemModulo :: Int
  }
  deriving (Generic)

initState :: Vector Monkey -> KeepAwayState
initState monkeys =
  KeepAwayState
    { monkeys = monkeys,
      inspectionCounts = V.replicate (length monkeys) 0,
      itemModulo = product $ fmap testDiv monkeys
    }

keepawayTurn :: Int -> Int -> State KeepAwayState ()
keepawayTurn worryDenom monkeyIdx = do
  monkey <- fmap fromJust $ preuse $ #monkeys . ix monkeyIdx
  itemModVal <- use $ #itemModulo
  forM_ (monkey ^. #items) $ \item -> do
    let newItem = (operation monkey item `mod` itemModVal) `div` worryDenom
        targetMonkeyIdx = test monkey newItem
    #inspectionCounts . ix monkeyIdx += 1
    #monkeys . ix targetMonkeyIdx . #items %= (|> newItem)
  #monkeys . ix monkeyIdx . #items .= mempty

keepaway :: Int -> Int -> State KeepAwayState ()
keepaway numRounds worryDenom = do
  replicateM_ numRounds $ do
    numMonkeys <- length <$> use #monkeys
    forM_ [0 .. numMonkeys -1] $ keepawayTurn worryDenom

runKeepaway :: Vector Monkey -> Int -> Int -> KeepAwayState
runKeepaway monkeys numRound worryDenom = execState (keepaway numRound worryDenom) $ initState monkeys

monkeyBusiness :: KeepAwayState -> Int
monkeyBusiness finalState =
  product $ take 2 $ reverse $ sort $ V.toList $ finalState ^. #inspectionCounts

main :: IO ()
main = do
  testMonkeys <- parseOrDie parseMonkeys <$> TIO.readFile "aoc22_day11_test"
  let testFinalState1 = runKeepaway testMonkeys 20 3
      monkeyItems = V.toList $ fmap (foldr (:) [] . items) $ testFinalState1 ^. #monkeys
      testFinalState2 = runKeepaway testMonkeys 10000 1
  assert
    ( monkeyItems == [[10, 12, 14, 26, 34], [245, 93, 53, 199, 115], [], []]
        && monkeyBusiness testFinalState1 == 10605
        && monkeyBusiness testFinalState2 == 2713310158
    )
    $ do
      monkeys <- parseOrDie parseMonkeys <$> TIO.readFile "aoc22_day11_input"
      let finalState1 = runKeepaway monkeys 20 3
      putStrLn $ "Question 1 answer is: " ++ show (monkeyBusiness finalState1)
      let finalState2 = runKeepaway monkeys 10000 1
      putStrLn $ "Question 2 answer is: " ++ show (monkeyBusiness finalState2)
