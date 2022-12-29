module Main (main) where

import Data.Graph.Inductive as G
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import GHC.Word
import Data.Bits
import Data.Array.Repa hiding ((++), zipWith)
import Data.Array.Repa qualified as R
import Data.Text.IO qualified as TIO
import Imports hiding (Empty, D)
import ParseUtils

data Tile = Empty | Wall | UpB | DownB | LeftB | RightB
    deriving (Generic, Eq, Ord, Show, Enum, Bounded)

bitMask :: Tile -> Word8
bitMask tile =
    case tile of
        Empty -> zeroBits
        _ -> bit $ fromEnum tile

-- Storing boards using bitmasks for efficiency and handling
-- of multiple blizzards on one tile
type Board = Array U DIM2 Word8

boardParser :: Parsec Void Text Board
boardParser = do
    rows <- some $ do
        row <- some $ do
            tileChar <- oneOf ("#.><^v" :: [Char])
            return $ case tileChar of
                '#' -> Wall
                '.' -> Empty
                '>' -> RightB
                '<' -> LeftB
                '^' -> UpB
                'v' -> DownB
                _ -> undefined
        newline
        return row
    return $ fromListUnboxed (ix2 (length rows) (length $ head rows)) $ bitMask <$> concat rows

testTile :: Word8 -> Tile -> Bool
testTile word tile =
    case tile of
        Empty -> word == zeroBits
        _ -> testBit word (fromEnum tile)

setTile :: Word8 -> Tile -> Word8
setTile word tile = setBit word (fromEnum tile)

clearTile :: Word8 -> Tile -> Word8
clearTile word tile =
    case tile of
        Empty -> word
        _ -> clearBit word (fromEnum tile)

up, down, left, right :: (Int, Int)
up = (-1,0)
down = (1,0)
left = (0,-1)
right = (0,1)

blizzardDir :: [(Tile, (Int, Int))]
blizzardDir = [
    (UpB, up),
    (DownB, down),
    (LeftB, left),
    (RightB, right)]

wrapCoord :: Int -> Int -> Int
wrapCoord width i
  | i <= 0 = width - 2 + i
  | i >= width - 1 = 1 + (i - (width - 1))
  | otherwise = i

stepBlizzards :: Board -> Board
stepBlizzards board =
    let (Z :. h :. w) = extent board
    in R.computeUnboxedS $ R.traverse board id
        $ \idxFn pos@(Z :. i :. j) ->
            let centerWord = idxFn pos
                wrapIdxFn (Z :. i :. j) = idxFn $ ix2 (wrapCoord h i) (wrapCoord w j)
                accumBlizzard (tile, (di, dj)) curWord =
                    -- The blizzard is:
                    -- * on that tile next if it is on the opposite tile now
                    -- * otherwise, it is gone from this tile if it is there already
                    -- * we wrap the coordinates to lookup on the other side when we
                    --   are next to the wall
                    let opWord = wrapIdxFn (ix2 (i-di) (j-dj))
                    in if testTile opWord tile
                        then setTile curWord tile
                        else clearTile curWord tile
            in if i <= 0 || j <= 0 || i >= h - 1 || j >= w - 1 then centerWord else
                foldr accumBlizzard centerWord blizzardDir

precomputeBlizzards :: Board -> [Board]
precomputeBlizzards startBoard =
    let helper curBoard =
            let nextBoard = stepBlizzards curBoard
                rest =
                    if nextBoard == startBoard
                        then []
                        else helper nextBoard
            in curBoard : rest
    in helper startBoard

countEmpty = runIdentity . sumAllP . R.map (\word -> if testTile word Empty then 1 else 0)

stackBoards :: [Board] -> Array D DIM3 Word8
stackBoards boards =
    let shape = extent $ head boards
        newShape = shape :. 1
    in foldr1 R.append $ fmap (reshape newShape) boards

-- Split horizontal from vertical blizzards
splitBlizzards :: Board -> (Board, Board)
splitBlizzards board =
    let verticalMask = complement $ foldr1 (.|.) [bitMask t | t <- [UpB, DownB]]
        horizontalMask = complement $ foldr1 (.|.) [bitMask t | t <- [LeftB, RightB]]
        getHorizontal = (.&. verticalMask)
        getVertical = (.&. horizontalMask)
    in (computeUnboxedS $ R.map getHorizontal board,
        computeUnboxedS $ R.map getVertical board)

mergeBlizzards b1 b2 = R.computeUnboxedS $ R.zipWith (.|.) b1 b2

data SimState = SimState {
    blizzards :: [(Int, Board)],
    curPos :: DIM2
} deriving (Generic)

initState :: Board -> SimState
initState board =
    let Z :. h :. w = extent board
    in SimState {
        blizzards = cycle $ zip [0..] (precomputeBlizzards board),
        curPos = ix2 0 1
    }

type Sim = State SimState

legalActions :: SimState -> [DIM2]
legalActions SimState {..} =
    let (_, nextBlizzard):_ = blizzards
        Z :. h :. w = extent nextBlizzard
        Z :. i :. j = curPos
    in [Z :. di :. dj |
        (di, dj) <- [up, down, left, right, (0,0)],
        let (i',j') = (i+di,j+dj) in i'>= 0 && j' >= 0 && i' < h && j' < w && testTile (nextBlizzard ! ix2 i' j') Empty]

performAction :: DIM2 -> Sim ()
performAction (Z :. di :. dj) = do
    #blizzards %= drop 1
    #curPos %= \(Z :. i :. j) -> Z :. i + di :. j + dj

type StateID = (Int, Int, Int)

stateId :: SimState -> StateID
stateId SimState {..} =
    let (blizzId, _):_ = blizzards
        (Z :. i :. j) = curPos
    in (blizzId, i, j)

data BuildGraphState = BGState {
    curNode :: Node,
    startPos :: DIM2,
    endPos :: DIM2,
    startNodes :: [Node],
    terminalNodes :: [Node],
    stateIdToNode :: Map StateID Node,
    curGraph :: Gr () Int
} deriving (Generic)

addNode :: Maybe SimState -> SimState -> State BuildGraphState Bool
addNode mParent simState = do
    let stateIdVal = stateId simState
    mNode <- use $ #stateIdToNode . at stateIdVal
    (childNode, isNew) <- case mNode of
        Just n -> return (n, False)
        Nothing -> do
            curNode <- use #curNode
            #stateIdToNode . at stateIdVal ?= curNode
            start <- (simState ^. #curPos ==) <$> use #startPos
            when start $ #startNodes %= (curNode:)
            terminal <- (simState ^. #curPos ==) <$> use #endPos
            when terminal $ #terminalNodes %= (curNode:)
            #curGraph %= insNode (curNode, ())
            #curNode += 1
            return (curNode, True)
    forM_ mParent $ \parent -> do
        parentNode <- fmap fromJust $ use $ #stateIdToNode . at (stateId parent)
        -- Every single edge costs 1 minute to traverse.
        #curGraph %= insEdge (parentNode, childNode, 1)
    return isNew

buildGraph' :: SimState -> State BuildGraphState ()
buildGraph' simState = do
    let actions = legalActions simState
    if null actions then do
        return ()
    else do
        forM_ actions $ \action -> do
            let newState = execState (performAction action) simState
            isNewNode <- addNode (Just simState) newState
            when isNewNode $ buildGraph' newState

buildGraph :: SimState -> State BuildGraphState ()
buildGraph startState = do
    void $ addNode Nothing startState
    buildGraph' startState

graphFromBoard :: Board -> (Node, [Node], [Node], Gr () Int)
graphFromBoard board =
    let startState = initState board
        Z :. h :. w = extent board
        BGState {..} = execState (buildGraph startState) $ BGState {
            curNode=0,
            startPos=ix2 0 1,
            endPos=ix2 (h-1) (w-2),
            startNodes=[],
            terminalNodes=[],
            stateIdToNode=mempty,
            curGraph=G.empty
        }
    in (0, startNodes, terminalNodes, curGraph)

shortestPathLength :: Board -> Int -> Int
shortestPathLength board numTrips =
    let (startNode, startNodes, terminalNodes, graph) = graphFromBoard board
    in shortestTripLength graph startNode terminalNodes startNodes numTrips - 1

shortestTripLength :: Gr () Int -> Node -> [Node] -> [Node] -> Int -> Int
shortestTripLength graph startNode endNodes startNodes numTrips =
    if numTrips == 0 then 0 else
        let shortestPaths = spTree startNode graph
            terminalPaths = [p | p <- unLPath <$> shortestPaths, fst (head p) `elem` endNodes]
            pathLength p = snd $ head p
            shortestTerminal = minimumBy (\a b -> compare (pathLength a) (pathLength b)) terminalPaths
            (endNode,_) = head shortestTerminal
        in pathLength shortestTerminal + shortestTripLength graph endNode startNodes endNodes (numTrips-1)

main :: IO ()
main = do
    -- Some tests
    testBoard <- parseOrDie boardParser <$> TIO.readFile "aoc22_day24_test"
    let (testHBoard, testVBoard) = splitBlizzards testBoard
        testAllHBoard = precomputeBlizzards testHBoard
        testAllVBoard = precomputeBlizzards testVBoard
        testExpectedAllStates = precomputeBlizzards testBoard
        testActualAllStates = zipWith mergeBlizzards testAllHBoard testAllVBoard
    assert (length testExpectedAllStates == 5) $ return ()
    let testNumEmpty :: Int = countEmpty $ stackBoards testExpectedAllStates
    assert (testNumEmpty == 126) $ return ()
    assert (testExpectedAllStates == testActualAllStates) $ return ()
    testBoard2 <- parseOrDie boardParser <$> TIO.readFile "aoc22_day24_test2"
    let (testHBoard, testVBoard) = splitBlizzards testBoard2
        testAllHBoard = precomputeBlizzards testHBoard
        testAllVBoard = precomputeBlizzards testVBoard
    assert (length testAllHBoard == 6) $ return ()
    assert (length testAllVBoard == 4) $ return ()
    let actualSPLength = shortestPathLength testBoard2 1
        actualSPLength3 = shortestPathLength testBoard2 3
    assert (actualSPLength == 18) $ return ()
    assert (actualSPLength3 == 54) $ return ()
    board <- parseOrDie boardParser <$> TIO.readFile "aoc22_day24_input"
    putStrLn $ "Question 1 answer is: " ++ show (shortestPathLength board 1)
    putStrLn $ "Question 2 answer is: " ++ show (shortestPathLength board 3)
