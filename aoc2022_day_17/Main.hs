{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Data.Hashable
import Data.IntMap qualified as IMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as ISet
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

type Rock = [(Int, Int)]

hBar :: Rock
hBar = [(0, 0), (1, 0), (2, 0), (3, 0)]

plus :: Rock
plus = [(1, 0), (1, 1), (1, 2), (0, 1), (2, 1)]

jRock :: Rock
jRock = [(2, 0), (2, 1), (2, 2), (0, 2), (1, 2)]

vBar :: Rock
vBar = [(0, 0), (0, 1), (0, 2), (0, 3)]

square :: Rock
square = [(0, 0), (0, 1), (1, 0), (1, 1)]

allRocks :: [Rock]
allRocks = [hBar, plus, jRock, vBar, square]

parseJet :: Parsec Void Text [Int]
parseJet = do
  jets <- some $ do
    jetChar <- oneOf ['<', '>']
    return $ case jetChar of
      '<' -> -1
      '>' -> 1
      _ -> error "This should never happen"
  void newline
  return jets

-- Simple row major encoding from (x, y) location to
-- a flat node in the graph for efficiency
pointNode :: (Int, Int) -> Int
pointNode (x, y) = 7 * y + x

nodePoint :: Int -> (Int, Int)
nodePoint i = (i `mod` 7, i `div` 7)

type EmptySpaceGraph = IntSet

data SimState = SimState
  { emptySpace :: !EmptySpaceGraph,
    rocks :: [(Int, Rock)],
    jets :: [(Int, Int)],
    minRockY :: !Int,
    towerSize :: !Int
  }
  deriving (Generic, Show)

initState :: [Int] -> SimState
initState jets =
  SimState
    { emptySpace = mempty,
      rocks = cycle (zip [0 ..] allRocks),
      jets = cycle (zip [0 ..] jets),
      minRockY = 1,
      towerSize = 0
    }

pop :: (Monad m) => Lens' SimState [(Int, a)] -> StateT SimState m a
pop stackLens = do
  (_, a) <- head <$> use stackLens
  stackLens %= tail
  return a

isEmpty :: (Int, Int) -> EmptySpaceGraph -> Bool
isEmpty = ISet.member . pointNode

addRock :: Rock -> EmptySpaceGraph -> EmptySpaceGraph
addRock rock (force -> !graph) = ISet.difference graph $ ISet.fromList $ fmap pointNode rock

padEmptySpace :: [(Int, Int)] -> EmptySpaceGraph -> EmptySpaceGraph
padEmptySpace positions (force -> !graph) = ISet.union graph $ ISet.fromList $ fmap pointNode positions

canMoveTo :: Rock -> (Int, Int) -> State SimState Bool
canMoveTo rock (x, y) = do
  emptySpace <- use #emptySpace
  return $
    all
      ( \(tx, ty) ->
          let x' = x + tx
              y' = y + ty
           in x' >= 0 && x' < 7 && y' <= 0 && isEmpty (x', y') emptySpace
      )
      rock

-- Simple DFS implementation.
filterConnected :: (Int, Int) -> EmptySpaceGraph -> EmptySpaceGraph
filterConnected point (force -> !graph) =
  execState (connected' (pointNode point, point)) mempty
  where
    connected' (curNodeFlat, (x, y)) = do
      visited <- get
      let neighbors =
            filter (\(p, _) -> ISet.member p graph && not (ISet.member p visited)) $
              fmap (\p -> (pointNode p, p)) [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
      modify $ ISet.insert curNodeFlat
      forM_ neighbors connected'

dropRock :: Rock -> (Int, Int) -> State SimState ()
dropRock rock curPos@(x, y) = do
  jet <- pop #jets
  let shiftPos = (x + jet, y)
  canShift <- canMoveTo rock shiftPos
  let (x', y') = if canShift then shiftPos else curPos
      dropPos = (x', y' + 1)
  canDrop <- canMoveTo rock dropPos
  let (x'', y'') = if canDrop then dropPos else (x', y')
  if canDrop
    then dropRock rock dropPos
    else do
      -- Can't drop it further so adding the positions to the tower.
      let newRockPos = [(x'' + tx, y'' + ty) | (tx, ty) <- rock]
      #emptySpace %= addRock newRockPos
      -- Updating min tower y and tower size
      let minRockY = minimum $ snd <$> newRockPos
      -- #minRockY %= min minRockY
      prevMinRockY <- use #minRockY
      newMinRockY <- #minRockY <.= min prevMinRockY minRockY
      #towerSize += prevMinRockY - newMinRockY

-- Taken straight from hashable source code (for newer versions that
-- do include this instance)
instance Hashable IntSet where
  hashWithSalt salt x =
    ISet.foldl'
      hashWithSalt
      (hashWithSalt salt (ISet.size x))
      x

dropRocks :: Int -> SimState -> IO SimState
dropRocks numRocks initState = do
  -- Forcing the empty space data structure every iteration to avoid space leaks
  -- leading to time leaks.
  let dropRock' (state@SimState {emptySpace = force -> !es, ..}, mPrevTime, stateHashes, i) = do
        if i == numRocks
          then return state
          else do
            res <- flip evalStateT state $ do
              -- Compute and store the state hash by hashing
              -- a triplet of:
              -- - rock index
              -- - jet index
              -- - empty space intset (with normalized Y coordinates)
              let (ri, _) = head rocks
                  (ji, _) = head jets
                  normEs = if ISet.null es then es else ISet.map (\(nodePoint -> (x, y)) -> pointNode (x, y - minRockY)) es
                  stHash = hash (ri, ji, normEs)
                  stateHashes' = IMap.insert stHash (i, towerSize) stateHashes
              case IMap.lookup stHash stateHashes of
                Just (oldIt, oldTowerSize) -> do
                  lift $ putStrLn $ "Same state detected between iterations " ++ show oldIt ++ " and " ++ show i ++ "!"
                  -- Once we have found an iteration with a period. We can fast forward in time as far as we want
                  -- by simply multiplying the towersize
                  let periodLength = i - oldIt
                      periodSizeIncrease = towerSize - oldTowerSize
                      numFFSteps = (numRocks - i) `div` periodLength
                      ffTowerSize = towerSize + periodSizeIncrease * numFFSteps
                      ffIt = i + periodLength * numFFSteps
                  lift $ putStrLn $ "Fast forwarding to iteration " ++ show ffIt
                  return (state & #towerSize .~ ffTowerSize, mPrevTime, mempty, ffIt)
                Nothing -> do
                  rock <- pop #rocks
                  minRockY <- use #minRockY
                  let startPos@(_, minY) = (2, minRockY - 4 - maximum (snd <$> rock))
                  -- Add empty tiles corresponding to the vertical space added to contain
                  -- the falling tile.
                  #emptySpace %= padEmptySpace [(x, y) | y <- [minY .. minRockY -1], x <- [0 .. 6]]
                  when (i `mod` 1000 == 0) $ do
                    -- Periodically emove unconnected empty tiles to keep memory usage in check.
                    #emptySpace %= filterConnected (0, minY)
                  st <- get
                  let st' = execState (dropRock rock (2, minY)) st
                  if i `mod` 100000 == 0
                    then do
                      lift $ putStrLn $ "Iteration " ++ show i ++ " of " ++ show numRocks
                      endTime <- lift getTime
                      forM_ mPrevTime $ \prevTime -> do
                        lift $ putStrLn $ "In " ++ show (endTime - prevTime) ++ " seconds "
                      return (st', Just endTime, stateHashes', i + 1)
                    else return (st', mPrevTime, stateHashes', i + 1)
            dropRock' res
  dropRock' (initState, Nothing, mempty, 1)

towerHeight :: [Int] -> Int -> IO Int
towerHeight jets numRocks = do
  res <- dropRocks numRocks $ initState jets
  return $ towerSize res

main :: IO ()
main = do
  testJets <- parseOrDie parseJet <$> TIO.readFile "aoc22_day17_test"
  actualHeight1 <- towerHeight testJets 2023
  actualHeight2 <- towerHeight testJets 1000000000001
  assert (actualHeight1 == 3068 && actualHeight2 == 1514285714288) $ do
    jets <- parseOrDie parseJet <$> TIO.readFile "aoc22_day17_input"
    height1 <- towerHeight jets 2023
    putStrLn $ "Question 1 answer is: " ++ show height1
    height2 <- towerHeight jets 1000000000001
    putStrLn $ "Question 2 answer is: " ++ show height2
