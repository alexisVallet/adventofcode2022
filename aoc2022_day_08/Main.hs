module Main where

import Data.Array.Repa hiding ((++))
import Data.Array.Repa qualified as R
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

parseTrees :: Parsec Void Text (Array U DIM2 Int)
parseTrees = do
  treeRows <- some $ someTill (digitToInt <$> digitChar) newline
  return $ fromListUnboxed (Z :. length treeRows :. length (head treeRows)) $ concat treeRows

fromAALines :: (Int -> [[Int]] -> a) -> DIM2 -> (DIM2 -> Int) -> DIM2 -> a
fromAALines processLines (Z :. h :. w) valAt center@(Z :. y :. x) =
  -- reversing top and bottom line so the first element of each list
  -- is always the closest to the center value. This matters for question 2 only.
  let top = reverse [Z :. y' :. x | y' <- [0 .. y -1]]
      bottom = [Z :. y' :. x | y' <- [y + 1 .. h -1]]
      left = reverse [Z :. y :. x' | x' <- [0 .. x -1]]
      right = [Z :. y :. x' | x' <- [x + 1 .. w -1]]
   in processLines (valAt center) [valAt <$> line | line <- [top, bottom, left, right]]

isVisible :: DIM2 -> (DIM2 -> Int) -> DIM2 -> Bool
isVisible = fromAALines $ \valAtCenter valAtLines ->
  or [valAtCenter > maximum (-1 : line) | line <- valAtLines]

scenicScore :: DIM2 -> (DIM2 -> Int) -> DIM2 -> Int
scenicScore = fromAALines $ \valAtCenter valAtLines ->
  let viewingDistance line = case line of
        [] -> 0
        (x : xs) -> 1 + if x >= valAtCenter then 0 else viewingDistance xs
   in product $ fmap viewingDistance valAtLines

numVisible :: Array U DIM2 Int -> Int
numVisible treesArray =
  runIdentity $
    sumAllP $
      R.map fromEnum $
        R.traverse treesArray id (isVisible $ extent treesArray)

maxScenicScore :: Array U DIM2 Int -> Int
maxScenicScore treesArray =
  runIdentity $
    foldAllP max 0 $
      R.traverse treesArray id (scenicScore $ extent treesArray)

main :: IO ()
main = do
  testTreesArray <- parseOrDie parseTrees <$> TIO.readFile "aoc22_day08_test"
  let actualNumVisible = numVisible testTreesArray
  let actualMaxScenicScore = maxScenicScore testTreesArray
  assert (actualNumVisible == 21 && actualMaxScenicScore == 8) $ do
    treesArray <- parseOrDie parseTrees <$> TIO.readFile "aoc22_day08_input"
    putStrLn $ "Question 1 answer is: " ++ show (numVisible treesArray)
    putStrLn $ "Question 2 answer is: " ++ show (maxScenicScore treesArray)
