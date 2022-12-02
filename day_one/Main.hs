module Main (main) where

import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Text (Text)
import qualified Data.Text.IO as TIO

inputParser :: Parsec Void Text [[Int]]
inputParser = many numberBlock

numberBlock :: Parsec Void Text [Int]
numberBlock = do
    numbers <- some $ do
        n <- decimal
        newline
        return n
    eof <|> do
        newline
        return ()
    return numbers

main :: IO ()
main = do
    contents <- TIO.readFile "input_1_1"
    case runParser inputParser "" contents of
        Left err -> print $ errorBundlePretty err
        Right calories -> do
            let sumCalories = [sum cs | cs <- calories]
            putStrLn $ "Max: " ++ show (maximum sumCalories)
            putStrLn $ "Top 3: " ++ show (sum $ take 3 $ reverse $ sort sumCalories)
