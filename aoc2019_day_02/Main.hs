module Main where

import IntCode
import Imports
import ParseUtils
import Data.Text.IO qualified as TIO

main :: IO ()
main = do
    program <- parseOrDie intCodeParser <$> TIO.readFile "aoc19_day02_input"
    let finalState = execIntCode program 12 2
    putStrLn $ "Question 1 answer is: " ++ show (finalState ^?! ix 0)
    let (n, v) = 
            fromJust 
            $ find (\(n, v) -> execIntCode program n v ^?! ix 0 == 19690720) 
            [(n, v) | n <- [0..99], v <- [0..99]]
    putStrLn $ "Question 2 answer is: " ++ show (100 * n + v)
