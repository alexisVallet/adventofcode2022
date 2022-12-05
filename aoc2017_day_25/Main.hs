module Main where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Text (Text)
import ParseUtils

data RuleSet = RuleSet {
    curState :: Char,
    stateRules :: Map Char StateRule
}

data StateRule = StateRule {
    zeroCase :: RuleCase,
    oneCase :: RuleCase
}

type Direction = Either () ()

data RuleCase = RuleCase {
    writeValue :: Bool,
    moveDirection :: Direction,
    nextState :: Char
}


ruleSetParser :: Parsec Void Text (RuleSet, Int)
ruleSetParser = do
    string "Begin in state "
    initialState <- letterChar
    string ".\n"
    string "Perform a diagnostic checksum after "
    numSteps <- decimal
    skipLines 2
    stateRules <- fmap Map.fromList $ many $ do
        string "In state "
        state <- letterChar
        skipLines 1
        let ruleParser = do
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
                return $ RuleCase {
                    writeValue=writeValue,
                    moveDirection=moveDirection,
                    nextState=nextState
                }
        zeroCase <- ruleParser
        oneCase <- ruleParser
        skipLines 1
        return (state, StateRule {
            zeroCase=zeroCase,
            oneCase=oneCase
        })
    let ruleSet = RuleSet {
            curState=initialState,
            stateRules=stateRules
        }
    return (ruleSet, numSteps)


data TuringMachine = TM {
    leftTape :: [Bool],
    readVal :: Bool,
    rightTape :: [Bool] 
}

initialize :: TuringMachine
initialize = TM [] False []

move :: Direction -> TuringMachine -> TuringMachine
move (Left ()) (TM (l:ls) c rs) = TM ls l (c:rs)
move (Left ()) (TM [] c rs) = TM [] False (c:rs)
move (Right ()) (TM ls c (r:rs)) = TM (c:ls) r rs
move (Right ()) (TM ls c []) = TM (c:ls) False []

writeVal :: Bool -> TuringMachine -> TuringMachine
writeVal b (TM ls _ rs) = TM ls b rs

main :: IO ()
main = undefined
