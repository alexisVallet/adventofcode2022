module Main where

import Data.Array.Repa hiding ((++))
import Data.Array.Repa qualified as R
import Data.Graph.Inductive
import Data.Graph.Inductive qualified as G
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Imports hiding (D, (!))
import ParseUtils

voxelParser :: Parsec Void Text (Array U DIM2 Int)
voxelParser = do
  voxels <- some $ do
    coords <- sepBy decimal (char ',')
    void newline
    return coords
  return $ fromListUnboxed (ix2 (length voxels) 3) $ concat voxels

pairwiseConnected :: Array U DIM2 Int -> Array U DIM2 Int -> Array D DIM2 Int
pairwiseConnected voxels1 voxels2 =
  let Z :. numVoxels1 :. _ = extent voxels1
      Z :. numVoxels2 :. _ = extent voxels2
      lVoxels = extend (Z :. All :. numVoxels2 :. All) voxels1
      rVoxels = extend (Z :. numVoxels1 :. All :. All) voxels2
      absDiff = R.map abs $ lVoxels -^ rVoxels
      manhattanDistMat = runIdentity $ sumP absDiff
   in R.map (\d -> if d == 1 then 1 else 0) manhattanDistMat

adjMat :: Array U DIM2 Int -> Array D DIM2 Int
adjMat voxels = pairwiseConnected voxels voxels

-- Consider the graph where nodes are voxels and there is an edge
-- iff the manhattan distance between voxels is 1 (i.e. they are adjacent).
-- Then the number of visible faces for each node is equal to 6 minus the
-- degree of the node.
numVisibleFaces :: Array D DIM2 Int -> Int
numVisibleFaces adjMat =
  let Z :. numVoxels :. _ = extent adjMat
      sumOfNodeDegrees = runIdentity $ sumAllP adjMat
   in numVoxels * 6 - sumOfNodeDegrees

-- Generate voxels for the empty locations, within the bounding box
-- of the lava voxels, + 1 element of padding in all directions (the outside air)
emptyVoxels :: Array U DIM2 Int -> Array U DIM2 Int
emptyVoxels lavaVoxels =
  let xyzSlices = [slice lavaVoxels (Z :. All :. i) | i <- ([0 .. 2] :: [Int])]
      [xMin, yMin, zMin] = fmap (runIdentity . foldAllP min maxBound) xyzSlices
      [xMax, yMax, zMax] = fmap (runIdentity . foldAllP max minBound) xyzSlices
      lavaVoxelSet = Set.fromList $ chunksOf 3 $ R.toList lavaVoxels
      emptyVoxelList =
        [ [x, y, z] | x <- [xMin -1 .. xMax + 1], y <- [yMin -1 .. yMax + 1], z <- [zMin -1 .. zMax + 1], not $ Set.member [x, y, z] lavaVoxelSet
        ]
   in R.fromListUnboxed (Z :. length emptyVoxelList :. 3) $ concat emptyVoxelList

voxelGraph :: Array U DIM2 Int -> Gr () ()
voxelGraph voxels =
  let voxAdjMat = runIdentity $ computeUnboxedP $ adjMat voxels
      Z :. numNodes :. _ = extent voxels
      nodeList = [(i, ()) | i <- [0 .. numNodes - 1]]
      edgeList =
        [ (i, j, ()) | i <- [0 .. numNodes - 1], j <- [0 .. numNodes - 1], voxAdjMat ! ix2 i j == 1
        ]
   in mkGraph nodeList edgeList

exteriorSurfaceArea :: Array U DIM2 Int -> Int
exteriorSurfaceArea lavaVoxels =
  let noLavaVoxels = emptyVoxels lavaVoxels
      noLavaGraph = voxelGraph noLavaVoxels
      exteriorCC = G.reachable 0 noLavaGraph
      exteriorCCArr = R.fromListUnboxed (Z :. length exteriorCC) exteriorCC
      exteriorVoxels =
        runIdentity $
          computeUnboxedP $
            fromFunction
              (Z :. length exteriorCC :. 3)
              (\(Z :. i :. j) -> noLavaVoxels ! ix2 (exteriorCCArr ! ix1 i) j)
      boundaryAdjMat = pairwiseConnected lavaVoxels exteriorVoxels
   in runIdentity $ sumAllP boundaryAdjMat

main :: IO ()
main = do
  testVoxels <- parseOrDie voxelParser <$> TIO.readFile "aoc22_day18_test"
  let actualNumVisibleFaces = numVisibleFaces . adjMat $ testVoxels
      actualExteriorArea = exteriorSurfaceArea testVoxels
  assert
    ( actualNumVisibleFaces == 64
        && actualExteriorArea == 58
    )
    $ do
      voxels <- parseOrDie voxelParser <$> TIO.readFile "aoc22_day18_input"
      putStrLn $ "Question 1 answer is: " ++ show (numVisibleFaces . adjMat $ voxels)
      putStrLn $ "Question 2 answer is: " ++ show (exteriorSurfaceArea voxels)
