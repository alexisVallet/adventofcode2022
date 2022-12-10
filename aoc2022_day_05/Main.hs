module Main where

import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Imports
import ParseUtils

-- A crate is represented as a list ordered from top to bottom.
type CrateStacks = Vector [Char]

moveCrates :: (MonadState CrateStacks m) => (forall a. [a] -> [a]) -> Int -> Int -> Int -> m ()
moveCrates crateOrder numCrates src dst = do
  (cratesToMove, rest) <- splitAt numCrates <$> use (ix src)
  ix src .= rest
  ix dst %= (crateOrder cratesToMove ++)

cratesParseAndMove :: (forall a. [a] -> [a]) -> Parsec Void Text CrateStacks
cratesParseAndMove crateOrder = do
  stackRows <- some $ do
    let crateParser = do
          char '['
          crate <- letterChar
          char ']'
          return $ Just crate
        emptyCellParser = do
          replicateM_ 3 $ char ' '
          return Nothing
    cratesOrEmpty <- some $
      try $ do
        out <- emptyCellParser <|> crateParser
        optional . try $ char ' '
        return out
    newline
    return $ V.fromList cratesOrEmpty
  when (null stackRows) $ fail "No rows could be paresed!"
  -- Stack up the crates from the bottom
  let stackCrates curRow curStacks =
        V.zipWith
          ( \curStack mCrate -> case mCrate of
              Just crate -> crate : curStack
              Nothing -> curStack
          )
          curStacks
          curRow
      crateStacks = foldr stackCrates (V.fromList $ replicate (length $ head stackRows) []) stackRows
  -- We don't actually need the next two lines
  replicateM_ 2 $ manyTill anySingle newline
  -- Then we parse and apply the move actions as we go, because why not.
  fmap snd $
    flip runStateT crateStacks $
      many $ do
        string "move "
        numCrates <- decimal
        string " from "
        src <- decimal
        string " to "
        dst <- decimal
        moveCrates crateOrder numCrates (src - 1) (dst - 1)
        newline

topCrates :: CrateStacks -> [Char]
topCrates = V.toList . fmap head

showCrates :: CrateStacks -> String
showCrates crateStacks =
  let sizes = fmap length crateStacks
      maxSize = maximum sizes
      paddedStacks =
        transpose $
          V.toList $
            V.zipWith
              (\size stack -> replicate (maxSize - size) ' ' ++ stack)
              sizes
              crateStacks
   in unlines paddedStacks

main :: IO ()
main = do
  testMovedCrates <- parseOrDie (cratesParseAndMove reverse) <$> TIO.readFile "input_5_test"
  assert (topCrates testMovedCrates == "CMZ") $ do
    fileContents <- TIO.readFile "input_5"
    let movedCrates1 = parseOrDie (cratesParseAndMove reverse) fileContents
    putStrLn $ "Question 1 answer is: " ++ topCrates movedCrates1
    let movedCrates2 = parseOrDie (cratesParseAndMove id) fileContents
    putStrLn $ "Question 2 answer is: " ++ topCrates movedCrates2
