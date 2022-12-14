module Main where

import Data.Text.IO qualified as TIO
import Imports hiding (contains)
import ParseUtils

type Range = (Integer, Integer)

sectionAssignmentParser :: Parsec Void Text [(Range, Range)]
sectionAssignmentParser = many $ do
  let rangeParser = do
        start <- decimal
        char '-'
        end <- decimal
        return (start, end)
  range1 <- rangeParser
  char ','
  range2 <- rangeParser
  newline
  return (range1, range2)

contains :: Range -> Range -> Bool
contains (s1, e1) (s2, e2) = s1 <= s2 && e1 >= e2

symContains :: Range -> Range -> Bool
symContains r1 r2 = contains r1 r2 || contains r2 r1

overlaps :: Range -> Range -> Bool
overlaps (s1, e1) (s2, e2) = not $ e1 < s2 || e2 < s1

main :: IO ()
main = do
  sectionAssignments <- parseOrDie sectionAssignmentParser <$> TIO.readFile "input_4"
  putStrLn $ "Question 1 answer is: " ++ show (sum $ fmap (fromEnum . uncurry symContains) sectionAssignments)
  putStrLn $ "Question 2 answer is: " ++ show (sum $ fmap (fromEnum . uncurry overlaps) sectionAssignments)
