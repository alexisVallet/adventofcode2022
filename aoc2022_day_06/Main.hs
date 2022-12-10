module Main where

import Data.Set qualified as Set
import Imports

findFirstUniqueSubseqIndex :: (Ord a) => Int -> [a] -> Maybe Int
findFirstUniqueSubseqIndex chunkSize as =
  fmap ((+ chunkSize) . fst . head) $
    find (\chunk -> length (Set.fromList (snd <$> chunk)) == chunkSize) $
      divvy chunkSize 1 $
        zip [0 ..] as

main :: IO ()
main = do
  let testActualQ1 = findFirstUniqueSubseqIndex 4 "bvwbjplbgvbhsrlpgdmjqwftvncz"
      testActualQ2 = findFirstUniqueSubseqIndex 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  assert (testActualQ1 == Just 5 && testActualQ2 == Just 19) $ do
    fileContents <- readFile "input_6"
    putStrLn $
      "Question 1 answer is: "
        ++ show (findFirstUniqueSubseqIndex 4 fileContents)
    putStrLn $
      "Question 2 answer is: "
        ++ show (findFirstUniqueSubseqIndex 14 fileContents)
