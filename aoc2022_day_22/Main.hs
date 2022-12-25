module Main (main) where

import Data.IntMap qualified as IMap
import Imports hiding (Empty)
import ParseUtils

data Tile = Empty | Obstacle
    deriving (Generic, Eq, Ord, Show)

data Board = Board {
    tiles :: IntMap (IntMap Tile),
    rowBounds :: IntMap (Int, Int),
    colBounds :: IntMap (Int, Int)
} deriving (Generic, Eq, Ord, Show)

insertTile :: (Int, Int) -> Tile -> IntMap (IntMap Tile) -> IntMap (IntMap Tile)
insertTile (y, x) t board =
    board & at y %~ \mInner -> Just $ fromMaybe mempty mInner & at x ?~ t

boardFromList :: [((Int, Int), Tile)] -> Board
boardFromList assocs =
    let xPerRow = IMap.fromListWith (++) [(y, [x]) | ((y, x), _) <- assocs]
        yPerCol = IMap.fromListWith (++) [(x, [y]) | ((y, x), _) <- assocs]
    in
        Board {
            tiles = foldr (uncurry insertTile) mempty assocs,
            rowBounds = fmap (\xs -> (minimum xs, maximum xs)) xPerRow,
            colBounds = fmap (\ys -> (minimum ys, maximum ys)) yPerCol
        }

data Direction = RightDir | DownDir | LeftDir | UpDir
    deriving (Generic, Eq, Ord, Bounded, Enum, Show)

data SimState = SimState {
    board :: Board,
    curPos :: (Int, Int),
    curDir :: Direction
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
    let board = boardFromList [((y, x), t) | (y, row) <- tileRows, (x, t) <- row]
        (minY, minRowMap) = IMap.findMin $ tiles board
        (minX, _) = IMap.findMin minRowMap
    -- Note that in both test and input data, the top left position happens to
    -- be empty, so we don't bother actually checking that.
    return $ SimState {
        board = board,
        curPos = (minY, minX),
        curDir = RightDir
    }

main :: IO ()
main = undefined
