module Main where

import Data.IntMap qualified as IMap
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

data Tile = Wall | Sand
  deriving (Generic, Show, Enum)

type TileIdx = Int

type MapState = IntMap (IntMap Tile)

data SimState = SimState
  { mapState :: MapState,
    mFloorY :: Maybe Int
  }
  deriving (Generic, Show)

parsePath :: Parsec Void Text [(Int, Int)]
parsePath = do
  sepBy ((,) <$> decimal <*> (char ',' >> decimal)) (string " -> ")

parseMapState :: Parsec Void Text MapState
parseMapState = fmap initMapState $
  some $ do
    pathCoords <- parsePath
    void newline
    return pathCoords

reorder :: (Int, Int) -> (Int, Int)
reorder (i, j) = if i <= j then (i, j) else (j, i)

addTile :: Tile -> (Int, Int) -> MapState -> MapState
addTile t (x, y) =
  IMap.alter alterX x
  where
    alterX mYmap = Just $ maybe (IMap.singleton y t) (IMap.insert y t) mYmap

isObstacle :: (Int, Int) -> MapState -> Bool
isObstacle (x, y) mapState = isJust $ do
  yMap <- IMap.lookup x mapState
  IMap.lookup y yMap

fromTo :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
fromTo (p1@(x1, y1), p2@(x2, y2)) =
  let (y1', y2') = reorder (y1, y2)
      flipPair = uncurry $ flip (,)
   in if x1 == x2
        then [(x1, y') | y' <- [y1' .. y2']]
        else flipPair <$> fromTo (flipPair p1, flipPair p2)

initMapState :: [[(Int, Int)]] -> MapState
initMapState walls =
  let listToPair [i, j] = (i, j)
      wallPairs = concatMap (fmap listToPair . divvy 2 1) walls
      accumWalls wall curState = foldr (addTile Wall) curState $ fromTo wall
   in foldr accumWalls mempty wallPairs

-- Find the "floor" Y coordinate the grain of sand at should fall to. Nothing
-- if there is no floor. Should be very efficient thanks to specialized IntMap
-- functions, and the nested IntMap data structure.
findFloor :: (Int, Int) -> State SimState (Maybe Int)
findFloor (x, y) = do
  mapState <- use #mapState
  return $ do
    yMap <- IMap.lookup x mapState
    let (_, below) = IMap.split y yMap
    (\i -> i - 1) . fst <$> IMap.lookupMin below

-- Drops the sand. Returns Nothing if it falls into the void, the new position otherwise.
dropSandUnit :: (Int, Int) -> State SimState (Maybe (Int, Int))
dropSandUnit p@(x, _) = do
  mFloorPos <- findFloor p
  case mFloorPos of
    Nothing -> do
      mFloorY' <- use #mFloorY
      case mFloorY' of
        Nothing -> return Nothing
        Just floorY -> do
          let newPos = (x, floorY - 1)
          #mapState %= addTile Sand newPos
          return $ Just newPos
    Just y' -> do
      mapState <- use #mapState
      let newPos =
            fromJust $
              join $
                find isJust $
                  [if isObstacle p' mapState then Nothing else Just p' | p' <- [(x - 1, y' + 1), (x + 1, y' + 1)]] ++ [Just (x, y')]
      if newPos == (x, y')
        then do
          #mapState %= addTile Sand newPos
          return $ Just newPos
        else do
          dropSandUnit newPos

runSim :: Int -> State SimState Int
runSim i = do
  mNewPos <- dropSandUnit (500, 0)
  if isNothing mNewPos
    then return i
    else
      if mNewPos == Just (500, 0)
        then return (i + 1)
        else runSim (i + 1)

unitsOfSandUntilEnd :: Bool -> MapState -> Int
unitsOfSandUntilEnd hasFloor mapState =
  let floorY = maximum (concatMap IMap.keys mapState) + 2
   in evalState (runSim 0) $
        SimState
          { mapState = mapState,
            mFloorY = if hasFloor then Just floorY else Nothing
          }

main :: IO ()
main = do
  testMap <- parseOrDie parseMapState <$> TIO.readFile "aoc22_day14_test"
  let actual1 = unitsOfSandUntilEnd False testMap
      actual2 = unitsOfSandUntilEnd True testMap
  print actual1
  print actual2
  assert (actual1 == 24 && actual2 == 93) $ do
    mapState <- parseOrDie parseMapState <$> TIO.readFile "aoc22_day14_input"
    putStrLn $ "Question 1 answer is: " ++ show (unitsOfSandUntilEnd False mapState)
    putStrLn $ "Question 2 answer is: " ++ show (unitsOfSandUntilEnd True mapState)
