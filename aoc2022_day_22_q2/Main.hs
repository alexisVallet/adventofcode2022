module Main (main) where

import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IMap
import Data.Graph.Inductive hiding ((&))
import Data.Graph.Inductive qualified as G
import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Repr.Vector as R
import Imports hiding (Empty)
import ParseUtils

data Face = Face {
    tiles :: Array U DIM2 Bool,
    corners :: Map CubeVertex DIM2,
    cubeEdges :: Map (Int, Int) (CubeVertex, CubeVertex)
} deriving (Generic)

type CubeVertex = Char

reorder :: Ord a => (a, a) -> (a, a)
reorder (x, y) = if x <= y then (x, y) else (y, x)

facesParser :: Int -> Parsec Void Text (IntMap Face)
facesParser size = do
    -- Parsing the boards
    rows <- some $ do
        row <- some $ do
            tileChar <- oneOf (" .#" :: [Char])
            return $ case tileChar of
                ' ' -> Nothing
                '#' -> Just True
                '.' -> Just False
                _ -> undefined
        void newline
        return row
    newline
    let numRows = length rows
        numCols = maximum $ fmap length rows
        paddedRows = fmap (\ts -> take numCols $ ts ++ repeat Nothing) rows
        boardArray = R.fromListVector (ix2 numRows numCols) $ concat paddedRows
        faceMap = IMap.fromList $ zip [0..] [
            R.computeUnboxedS $ R.map fromJust $ extract (ix2 (i * size) (j * size)) (ix2 ((i + 1) * size) ((j + 1) * size)) boardArray
            | i <- [0..numRows-1], j <- [1..numRows-1], isJust (boardArray ! ix2 (i * size) (j * size))]
        cornerLocs = [ix2 0 0, ix2 0 (size-1), ix2 (size-1) 0, ix2 (size-1) (size-1)]
    -- Parsing corner specifications in the extended format
    faceCorners <- fmap IMap.fromList $ some $ do
        faceIdx <- decimal
        void $ char ':'
        corners@[a,b,c,d] <- some $ oneOf ['A'..'H']
        void newline
        return (faceIdx, (Map.fromList $ zip corners cornerLocs, Map.fromList [(up, (a,b)), (left, (a,c)), (right, (b,d)), (down,(c,d))]))
    -- Putting it all together
    return $ IMap.fromList [
        (i, Face tiles corners cubeEdges) 
        | ((i, tiles), (corners, cubeEdges)) <- zip (IMap.assocs faceMap) (IMap.elems faceCorners)]

-- Represent the board by a graph, with edges labelled with
-- their direction.
type Board = Gr () (Int, Int)

up, down, left, right :: (Int, Int)
up = (-1, 0)
down= (1, 0)
left = (0, -1)
right = (0, 1)

facesToBoard :: IntMap Face -> Board
facesToBoard faces =
    let -- We start by associating a unique graph node key to every location in each face
        faceToNodeKeys (faceIdx, Face {tiles=tiles}) =
            let Z :. h :. w = extent tiles
            in [(faceIdx, ix2  i j) | i <- [0..h-1], j <- [0..w-1], not $ tiles ! ix2 i j]
        nodeMap = Map.fromList $ zip (concatMap faceToNodeKeys $ IMap.assocs faces) [0..]
        -- We then compute the "inner" edges between graph nodes within each face
        faceToInnerEdges (faceIdx, Face {tiles=tiles}) =
            concat $ R.toList $ R.computeVectorS $ R.traverse tiles id $ \_ pos@(Z :. i :. j) ->
                case Map.lookup (faceIdx, pos) nodeMap of
                    Nothing -> []
                    Just srcNode ->
                            (\(dir, dstNode) -> (srcNode, dstNode, dir)) <$> mapMaybe
                            (\dir@(di, dj) ->
                                let pos' = ix2 (i + di) (j + dj)
                                    mNode = Map.lookup (faceIdx, pos') nodeMap
                                in mNode <&> (dir,))
                            [up, down, left, right]
        innerEdges = concatMap faceToInnerEdges $ IMap.assocs faces
        -- Then we compute the "outer" graph edges linking faces together, by
        -- considering common cube edges between all pairs of faces, and adding
        -- graph edges node by node along those common cube edges.
        iterateCoords face (c1, c2) =
            let (Z :. i1 :. j1) = face ^?! #corners . ix c1
                (Z :. i2 :. j2) = face ^?! #corners . ix c2
            in if i1 == i2 then 
                [Z :. i1 :. j | j <- [j1,j1 + signum (j2 - j1)..j2]]
            else
                [Z :. i :. j1 | i <- [i1,i1 + signum (i2 - i1)..i2]]
        outerEdges = 
            concatMap 
                (\((i1, f1), (i2, f2)) ->
                    let commonEdges = [(e1, e2) | e1 <- f1 ^. #cubeEdges, e2 <- f2 ^. #cubeEdges, reorder e1 == reorder e2]
                    in concatMap
                        (\(e1, e2) ->
                            mapMaybe (\(pos1, pos2) -> do
                                n1 <- Map.lookup (i1, pos1) nodeMap
                                n2 <- Map.lookup (i2, pos2) nodeMap
                                return (n1, n2, undefined))
                            $ zip (iterateCoords f1 e1) (iterateCoords f2 e2)) 
                        commonEdges)
                [(f1, f2) | f1 <- IMap.assocs faces, f2 <- IMap.assocs faces, fst f1 /= fst f2]
        edges = innerEdges ++ outerEdges
    in mkGraph ((,()) <$> Map.elems nodeMap) edges

main :: IO ()
main = undefined
