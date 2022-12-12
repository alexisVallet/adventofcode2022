module Main where

import Data.Array.Repa hiding ((++))
import Data.Array.Repa qualified as R
import Data.Array.Repa.Repr.Vector
import Data.Graph.Inductive
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

-- Terrain as a graph where nodes are labelled with height, and edges
-- with distance (1) if they are connected.
type Terrain = Gr (NodeType, Int) Int

data NodeType = Start | End | Other
  deriving (Generic, Show, Eq)

charToNodeLabel :: Char -> (NodeType, Int)
charToNodeLabel c =
  let itemIdx = fromEnum c
   in if fromEnum 'a' <= itemIdx && itemIdx <= fromEnum 'z'
        then (Other, itemIdx - fromEnum 'a')
        else case c of
          'S' -> (Start, 0)
          'E' -> (End, fromEnum 'z' - fromEnum 'a')
          _ -> error $ "unexpected char " ++ show c

idxToNode :: Int -> DIM2 -> Node
idxToNode width (Z :. y :. x) = width * y + x

heightToNodeEdges :: DIM2 -> (DIM2 -> (NodeType, Int)) -> DIM2 -> (LNode (NodeType, Int), [LEdge Int])
heightToNodeEdges (Z :. height :. width) idx pos@(Z :. y :. x) =
  let heightAt = snd . idx
      curNode = idxToNode width pos
      neighborCoords =
        [ ix2 y' x'
          | (y', x') <- [(y -1, x), (y + 1, x), (y, x -1), (y, x + 1)],
            y' >= 0 && y' < height && x' >= 0 && x' < width,
            heightAt (ix2 y' x') <= heightAt pos + 1
        ]
   in ((curNode, idx pos), [(curNode, idxToNode width nc, 1) | nc <- neighborCoords])

terrainParser :: Parsec Void Text (Terrain, Node, Node)
terrainParser = do
  nodeChars <- some $ do
    c <- some letterChar
    void newline
    return c
  let mapShape = Z :. length nodeChars :. length (head nodeChars)
      nodeCharArr = fromListUnboxed mapShape $ concat nodeChars
      nodeHeight = R.map charToNodeLabel nodeCharArr
      nodeAndEdges :: Array V DIM2 (LNode (NodeType, Int), [LEdge Int])
      nodeAndEdges = runIdentity $ R.computeP $ R.traverse nodeHeight id (heightToNodeEdges mapShape)
      (nodeList, edgeList) = second concat . unzip . R.toList $ nodeAndEdges
      startNode = fst . fromJust . find (\(_, (nt, _)) -> nt == Start) $ nodeList
      endNode = fst . fromJust . find (\(_, (nt, _)) -> nt == End) $ nodeList
  return (mkGraph nodeList edgeList, startNode, endNode)

shortestZeroStartPathLengthTo :: Terrain -> Node -> Maybe Int
shortestZeroStartPathLengthTo g end =
  let zeroElevationNodes = fst <$> filter (\(_, (_, e)) -> e == 0) (labNodes g)
      spLengths = catMaybes $ (\n -> spLength n end g) <$> zeroElevationNodes
   in case spLengths of
        [] -> Nothing
        _ -> Just $ minimum spLengths

main :: IO ()
main = do
  (testGraph, testStart, testEnd) <- parseOrDie terrainParser <$> TIO.readFile "aoc22_day12_test"
  let actualPathLength1 = spLength testStart testEnd testGraph
      actualPathLength2 = shortestZeroStartPathLengthTo testGraph testEnd
  assert
    ( actualPathLength1 == Just 31
        && actualPathLength2 == Just 29
    )
    $ do
      (graph, start, end) <- parseOrDie terrainParser <$> TIO.readFile "aoc22_day12_input"
      putStrLn $ "Question 1 answer is: " ++ show (spLength start end graph)
      putStrLn $ "Question 2 answer is: " ++ show (shortestZeroStartPathLengthTo graph end)
