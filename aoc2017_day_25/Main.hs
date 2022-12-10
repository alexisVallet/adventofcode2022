module Main where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

data RuleSet = RuleSet
  { curState :: Char,
    stateRules :: Map Char StateRule
  }
  deriving (Generic, Show)

data StateRule = StateRule
  { zeroCase :: RuleCase,
    oneCase :: RuleCase
  }
  deriving (Generic, Show)

type Direction = Either () ()

data RuleCase = RuleCase
  { writeValue :: Bool,
    moveDirection :: Direction,
    nextState :: Char
  }
  deriving (Generic, Show)

ruleSetParser :: Parsec Void Text (RuleSet, Int)
ruleSetParser = do
  string "Begin in state "
  initialState <- letterChar
  skipLines 1
  string "Perform a diagnostic checksum after "
  numSteps <- decimal
  skipLines 1
  stateRules <- fmap Map.fromList $
    many $ do
      skipLines 1
      string "In state "
      state <- letterChar
      skipLines 1
      let ruleParser = do
            skipLines 1
            string "    - Write the value "
            writeValue <- toEnum . read . (: []) <$> binDigitChar
            skipLines 1
            string "    - Move one slot to the "
            moveDirection <-
              (string "right" >> return (Right ()))
                <|> (string "left" >> return (Left ()))
            skipLines 1
            string "    - Continue with state "
            nextState <- letterChar
            skipLines 1
            return $
              RuleCase
                { writeValue = writeValue,
                  moveDirection = moveDirection,
                  nextState = nextState
                }
      zeroCase <- ruleParser
      oneCase <- ruleParser
      return
        ( state,
          StateRule
            { zeroCase = zeroCase,
              oneCase = oneCase
            }
        )
  let ruleSet =
        RuleSet
          { curState = initialState,
            stateRules = stateRules
          }
  return (ruleSet, numSteps)

data TuringMachine = TM
  { leftTape :: [Bool],
    readVal :: Bool,
    rightTape :: [Bool]
  }
  deriving (Generic)

checksum :: TuringMachine -> Integer
checksum (TM ls c rs) = sum $ fmap (fromIntegral . fromEnum) $ c : ls ++ rs

initTape :: TuringMachine
initTape = TM [] False []

pureMove :: Direction -> TuringMachine -> TuringMachine
pureMove (Left ()) (TM (l : ls) c rs) = TM ls l (c : rs)
pureMove (Left ()) (TM [] c rs) = TM [] False (c : rs)
pureMove (Right ()) (TM ls c (r : rs)) = TM (c : ls) r rs
pureMove (Right ()) (TM ls c []) = TM (c : ls) False []

writeVal :: Bool -> State MachineState ()
writeVal b = do
  #tape . #readVal .= b

data MachineState = MachineState
  { ruleSet :: RuleSet,
    tape :: TuringMachine
  }
  deriving (Generic)

diagnosticChecksum :: Int -> State MachineState Integer
diagnosticChecksum numIterations = do
  forM_ [1 .. numIterations] $ \_ -> do
    curStateVal <- use $ #ruleSet . #curState
    curRule <- fromJust <$> use (#ruleSet . #stateRules . at curStateVal)
    curVal <- use $ #tape . #readVal
    let ruleCase = curRule ^. if curVal then #oneCase else #zeroCase
    writeVal $ ruleCase ^. #writeValue
    #tape %= pureMove (ruleCase ^. #moveDirection)
    #ruleSet . #curState .= ruleCase ^. #nextState
  finalTape <- use #tape
  return $ checksum finalTape

initMachineState :: RuleSet -> MachineState
initMachineState ruleSet =
  MachineState
    { ruleSet = ruleSet,
      tape = initTape
    }

checksumFromFile :: String -> IO Integer
checksumFromFile fname = do
  (ruleSet, numIteration) <- parseOrDie ruleSetParser <$> TIO.readFile fname
  return $ evalState (diagnosticChecksum numIteration) $ initMachineState ruleSet

main :: IO ()
main = do
  testChecksum <- checksumFromFile "aoc17_day25_test"
  assert (testChecksum == 3) $ do
    checksum <- checksumFromFile "aoc17_day25"
    putStrLn $ "Question 1 answer is: " ++ show checksum
