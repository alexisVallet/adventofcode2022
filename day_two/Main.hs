module Main (main) where

import Data.Void
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Exception

data RPSPlay = Rock | Paper | Scissors
    deriving (Eq, Ord, Show, Enum, Bounded)

data Outcome = Win | Loss | Draw
    deriving (Eq, Ord, Show, Enum, Bounded)

charToPlay :: Char -> RPSPlay
charToPlay c =
    case c of
        'A' -> Rock
        'B' -> Paper
        'C' -> Scissors
        'X' -> Rock
        'Y' -> Paper
        'Z' -> Scissors
        _ -> error $ "Invalid play char " ++ show c

charToOutcome :: Char -> Outcome
charToOutcome c =
    case c of
        'X' -> Loss
        'Y' -> Draw
        'Z' -> Win
        _ -> error $ "Invalid outcome char " ++ show c

rpsStrategyParserV1 :: Parsec Void Text [(RPSPlay, RPSPlay)]
rpsStrategyParserV1 = many $ do
    play1 <- charToPlay <$> (char 'A' <|> char 'B' <|> char 'C')
    hspace
    play2 <- charToPlay <$> (char 'X' <|> char 'Y' <|> char 'Z')
    _ <- newline
    return (play1, play2)

rpsStrategyParserV2 :: Parsec Void Text [(RPSPlay, Outcome)]
rpsStrategyParserV2 = many $ do
    play <- charToPlay <$> (char 'A' <|> char 'B' <|> char 'C')
    hspace
    outcome <- charToOutcome <$> (char 'X' <|> char 'Y' <|> char 'Z')
    _ <- newline
    return (play, outcome)

opposite :: Outcome -> Outcome
opposite outcome = case outcome of
    Win -> Loss
    Draw -> Draw
    Loss -> Win

outcome :: (RPSPlay, RPSPlay) -> Outcome
outcome plays =
    case plays of
        (Rock, Rock) -> Draw
        (Rock, Paper) -> Win
        (Rock, Scissors) -> Loss
        (Paper, Paper) -> Draw
        (Paper, Scissors) -> Win
        (Scissors, Scissors) -> Draw
        (p1, p2) -> opposite $ outcome (p2, p1) -- Works thanks to symmetry

score :: (RPSPlay, RPSPlay) -> Integer
score plays@(_, play1) =
    let playScore = case play1 of
            Rock -> 1
            Paper -> 2
            Scissors -> 3
        outcomeScore = case outcome plays of
            Win -> 6
            Draw -> 3
            Loss -> 0
    in playScore + outcomeScore

playOutcomeToPlay :: Map (RPSPlay, Outcome) RPSPlay
playOutcomeToPlay = Map.fromList [((p1, outcome (p1, p2)), p2) | p1 <- [minBound..maxBound], p2 <- [minBound..maxBound]]

parseOrDie :: Parsec Void Text a -> Text -> a
parseOrDie parser contents = case runParser parser "" contents of
    Left err -> error $ errorBundlePretty err
    Right out -> out

scoresForFileV1 :: Text -> [Integer]
scoresForFileV1 contents = score <$> parseOrDie rpsStrategyParserV1 contents

main :: IO ()
main = do
    testScores <- scoresForFileV1 <$> TIO.readFile "input_2_test"
    assert (testScores == [4, 8, 3, 1, 5, 9, 7, 2, 6]) $ do
        inputContents <- TIO.readFile "input_2"
        let scoresV1 = scoresForFileV1 inputContents
        putStrLn $ "Question 1 total score: " ++ show (sum scoresV1)
        let scoresV2 = (\(play1, out) -> score (play1, playOutcomeToPlay ! (play1, out))) <$> parseOrDie rpsStrategyParserV2 inputContents
        putStrLn $ "Question 2 total score: " ++ show (sum scoresV2)
