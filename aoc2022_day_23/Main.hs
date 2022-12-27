module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

type Elves = Set (Int, Int)

elvesParser :: Parsec Void Text Elves
elvesParser = do
  rows <- fmap (zip [0 ..]) $
    some $ do
      row <- fmap (zip [0 ..]) $
        some $ do
          tileChar <- oneOf (".#" :: [Char])
          return $ case tileChar of
            '.' -> False
            '#' -> True
            _ -> undefined
      void newline
      return row
  return $ Set.fromList [(i, j) | (i, row) <- rows, (j, isElf) <- row, isElf]

data SimState = SimState
  { elves :: Elves,
    directions :: [[(Int, Int)]]
  }
  deriving (Generic, Eq, Ord, Show)

north, south, west, east :: (Int, Int)
north = (-1, 0)
south = (1, 0)
west = (0, -1)
east = (0, 1)

initState :: Elves -> SimState
initState elves =
  SimState
    { elves = elves,
      directions = divvy 4 1 $ cycle [north, south, west, east]
    }

type Sim = State SimState

popDirections :: Sim [(Int, Int)]
popDirections = do
  allDirs <- use #directions
  case allDirs of
    (directions : rest) -> do
      #directions .= rest
      return directions
    _ -> undefined

allDirs :: [(Int, Int)]
allDirs = [(dy, dx) | dy <- [-1 .. 1], dx <- [-1 .. 1], (dy, dx) /= (0, 0)]

withDiag :: (Int, Int) -> [(Int, Int)]
withDiag (dy, 0) = [(dy, 0), (dy, -1), (dy, 1)]
withDiag (0, dx) = [(0, dx), (-1, dx), (1, dx)]

elfRound :: Sim ()
elfRound = do
  directions <- popDirections
  elves <- use #elves
  let proposals =
        Map.fromList
          [(e, makeProposal e) | e <- Set.toList elves]
      makeProposal (y, x) =
        let neighbors = Set.fromList [dir | dir@(dy, dx) <- allDirs, Set.member (y + dy, x + dx) elves]
            valid = and . fmap (not . flip Set.member neighbors) . withDiag
         in if null neighbors then Nothing else find valid directions
      solveConflicts e@(y, x) =
        let proposalOffset = proposals ^?! ix e
         in case proposalOffset of
              Nothing -> e
              Just (dy, dx) ->
                let proposal = (y + dy, x + dx)
                 in case join $ Map.lookup (y + 2 * dy, x + 2 * dx) proposals of
                      Nothing -> proposal
                      Just (dy', dx') -> if (dy, dx) == (- dy', - dx') then e else proposal
  #elves %= Set.map solveConflicts

numEmptyAfter :: Int -> Elves -> Int
numEmptyAfter numRounds initElves =
  let finalElves = elves $ execState (replicateM numRounds elfRound) $ initState initElves
      ys = Set.map fst finalElves
      xs = Set.map snd finalElves
      area = (maximum ys - minimum ys + 1) * (maximum xs - minimum xs + 1)
   in -- The number of empty tiles is the area of the bounding box of the elves minus
      -- the number of elves.
      area - Set.size finalElves

findEndRound :: Int -> Sim Int
findEndRound i = do
  elvesBefore <- use #elves
  elfRound
  elvesAfter <- use #elves
  if elvesBefore == elvesAfter
    then return i
    else findEndRound (i + 1)

main :: IO ()
main = do
  testInitElves <- parseOrDie elvesParser <$> TIO.readFile "aoc22_day23_test"
  let actualNumEmpty = numEmptyAfter 10 testInitElves
  assert (actualNumEmpty == 110) $ return ()
  initElves <- parseOrDie elvesParser <$> TIO.readFile "aoc22_day23_input"
  putStrLn $ "Question 1 answer is: " ++ show (numEmptyAfter 10 initElves)
  putStrLn $ "Question 2 answer is: " ++ show (evalState (findEndRound 1) $ initState initElves)