module Main where

import Data.Void
import ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Text (Text)
import Data.Text.IO qualified as TIO

subjectNumber :: Int
subjectNumber = 7

cryptoStep :: Int -> Int
cryptoStep n = (n * subjectNumber) `mod` 20201227

findLoopNumber :: Int -> Int -> Int -> Int
findLoopNumber i n target =
    let stepResult = cryptoStep n
    in if stepResult == target then i else findLoopNumber (i + 1) stepResult target

keyParser :: Parsec Void Text (Int, Int)
keyParser = do
    k1 <- decimal
    _ <- newline
    k2 <- decimal
    return (k1, k2)

main :: IO ()
main = do
    (k1, k2) <- parseOrDie keyParser <$> TIO.readFile "aoc20_day25"
    let loopNumber1 = findLoopNumber 1 subjectNumber k1
        loopNumber2 = findLoopNumber 1 subjectNumber k2
        ek1 = iterate cryptoStep k1 !! loopNumber2
        ek2 = iterate cryptoStep k2 !! loopNumber1
    putStrLn $ "For public keys " ++ show k1 ++ " and " ++ show k2
    putStrLn $ "Found encryption keys " ++ show ek1 ++ " and " ++ show ek2
    putStrLn $ "Using loop numbers " ++ show loopNumber1 ++ " and " ++ show loopNumber2
