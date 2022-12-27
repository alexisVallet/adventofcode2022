module Main (main) where

import Data.IntMap qualified as IMap
import Data.Text.IO qualified as TIO
import Imports hiding (Empty)
import ParseUtils

data Tile = Empty | Obstacle
    deriving (Generic, Eq, Ord, Show)

data Tiles = Tiles {
    tiles :: IntMap (IntMap ()),
    bounds :: IntMap (Int, Int)
} deriving (Generic, Eq, Ord, Show)

type Board = (Tiles, Tiles)

insertTile :: (Int, Int) -> IntMap (IntMap ()) -> IntMap (IntMap ())
insertTile (y, x) board =
    board & at y %~ \mInner -> Just $ fromMaybe mempty mInner & at x ?~ ()

boardFromList :: [((Int, Int), Tile)] -> Board
boardFromList assocs =
    let xPerRow = IMap.fromListWith (++) [(y, [x]) | ((y, x), _) <- assocs]
        yPerCol = IMap.fromListWith (++) [(x, [y]) | ((y, x), _) <- assocs]
        obstacles = fst <$> filter ((== Obstacle) . snd) assocs
    in  {- essentially storing 2 copies of the map:
         * One split over the Y axis then X axis (quick lookup of next horizontal obstacles)
         * One split over the X axis then Y axis -}
        (Tiles {
            tiles = foldr insertTile mempty obstacles,
            bounds = fmap (\xs -> (minimum xs, maximum xs)) xPerRow
        }, Tiles {
            tiles = foldr insertTile mempty $ fmap (uncurry $ flip (,)) obstacles,
            bounds = fmap (\ys -> (minimum ys, maximum ys)) yPerCol
        })

data SimState = SimState {
    board :: Board,
    curPos :: (Int, Int),
    curDir :: (Int, Int)
} deriving (Generic, Eq, Ord, Show)

boardParser :: Parsec Void Text SimState
boardParser = do
    tileRows <- fmap (zip [1..]) $ some $ do
        row <- fmap (catMaybes . zipWith (\i mTile -> fmap (i,) mTile) [1..]) $ some $ do
            tileChar <- oneOf (" .#" :: [Char])
            return $ case tileChar of
                ' ' -> Nothing
                '.' -> Just Empty
                '#' -> Just Obstacle
                _ -> error "This should never happen"
        void newline
        return row
    let board = boardFromList [((y, x), t) | (y, row') <- tileRows, (x, t) <- row']
        (minX, _) = board ^?! _1 . #bounds . ix 1
    -- Note that in both test and input data, the top left position happens to
    -- be empty, so we don't bother actually checking that.
    return $ SimState {
        board = board,
        curPos = (1, minX),
        curDir = (0, 1)
    }

data Turn = Turn {
    side :: Either () (),
    next :: Move
} deriving (Generic, Eq, Ord, Show)

data Move = Move {
    amount :: Int,
    next :: Maybe Turn
} deriving (Generic, Eq, Ord, Show)

moveParser :: Parsec Void Text Move
moveParser = do
    amount <- decimal
    turn <- turnParser
    return $ Move {
        amount=amount,
        next=turn
    }

charToSide :: Char -> Either () ()
charToSide 'L' = Left ()
charToSide 'R' = Right ()
charToSide _ = error "should never happen!"

turnParser :: Parsec Void Text (Maybe Turn)
turnParser = do
    maybeSide <- Just . charToSide <$> oneOf ("LR" :: [Char]) <|> (newline >> return Nothing)
    case maybeSide of
        Nothing -> return Nothing
        Just side -> do
            move <- moveParser
            return $ Just $ Turn {
                side=side,
                next=move
            }

fullParser = (,) <$> boardParser <*> (newline >> moveParser)

type Sim = State SimState

nextObstacle :: (forall a . Lens' (a, a) a) -> (forall a . Lens' (a, a) a) -> Bool -> (Int, Int) -> Board -> Maybe (Int, Int)
nextObstacle y x front location board = do
    row <- IMap.lookup (location ^. y) (board ^. y . #tiles)
    let lookupFn = if front then IMap.lookupGE else IMap.lookupLE
    case lookupFn (location ^. x) row of
        Just (x', _) -> Just $ location & x .~ x'
        Nothing -> do
            xBounds <- IMap.lookup (location ^. y) (board ^. y . #bounds)
            nextObstacle y x front (location & x .~ (if front then fst else snd) xBounds) board

step :: (Int, Int) -> Int -> Int -> Maybe Int -> Int -> Int
step bounds@(minX, maxX) dx amount mObstacle curX =
    if amount == 0 then curX else
        let newX = curX + dx
            wrappedNewX = case (newX < minX, newX > maxX) of
                (False, False) -> newX
                (True, _) -> maxX
                (_, True) -> minX
        in if Just wrappedNewX == mObstacle then curX else step bounds dx (amount - 1) mObstacle wrappedNewX

performMove :: Move -> Sim ()
performMove (Move amount mNextTurn) = do
    curPos' <- use #curPos
    dir@(dy, dx) <- use #curDir
    board' <- use #board
    let (y, x) :: (forall a. Lens' (a, a) a, forall a. Lens' (a, a) a) = if dy == 0 then (_1, _2) else (_2, _1)
        mObstacle = nextObstacle y x (dir ^. x > 0) curPos' board'
        bounds' = board' ^?! y . #bounds . ix (curPos' ^. y)
    #curPos . x %= step bounds' (dir ^. x) amount ((^. x) <$> mObstacle)
    case mNextTurn of
        Nothing -> return ()
        Just (Turn side nextMove) -> do
            #curDir .= case side of
                Left _ -> (-dx, dy)
                Right _ -> (dx, -dy)
            performMove nextMove

runSim :: SimState -> Move -> SimState
runSim initState move = execState (performMove move) initState

password :: SimState -> Move -> Int
password initState move =
    let finalState = runSim initState move
        (finalRow, finalCol) = finalState ^. #curPos
        facing = case finalState ^. #curDir of
            (0, 1) -> 0
            (1, 0) -> 1
            (0, -1) -> 2
            (-1, 0) -> 3
            _ -> error $ "Invalid direction " ++ show (finalState ^. #curDir)
    in 1000 * finalRow + 4 * finalCol + facing

main :: IO ()
main = do
    (testInitState, testMove) <- parseOrDie fullParser <$> TIO.readFile "aoc22_day22_test"
    let actualPassword = password testInitState testMove
    assert (actualPassword == 6032) $ return ()
    (initState, move) <- parseOrDie fullParser <$> TIO.readFile "aoc22_day22_input"
    putStrLn $ "Question 1 answer is " ++ show (password initState move)
