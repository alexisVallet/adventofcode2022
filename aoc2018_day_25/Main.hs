module Main where

import Control.Exception
import Data.Array.Repa (All (..), Array, DIM2, U, Z (..), ix2, (-^), (:.) (..))
import Data.Array.Repa qualified as R
import Data.Graph
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Vector.Unboxed qualified as VU
import Data.Void
import ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

coordParser :: Parsec Void Text (Array U DIM2 Int)
coordParser = do
  coordList <- many $ do
    n1 <- signed space decimal
    _ <- char ','
    n2 <- signed space decimal
    _ <- char ','
    n3 <- signed space decimal
    _ <- char ','
    n4 <- signed space decimal
    _ <- newline
    return [n1, n2, n3, n4]
  let numPoints = length coordList
  return $ R.fromListUnboxed (ix2 numPoints 4) $ concat coordList

adjacencyMatrix :: (Monad m) => Array U DIM2 Int -> m (Array U DIM2 Bool)
adjacencyMatrix points = do
  let Z :. numPoints :. _ = R.extent points
      leftExtended = R.extend (Z :. All :. numPoints :. All) points
      rightExtended = R.extend (Z :. numPoints :. All :. All) points
      absDiff = R.map abs $ leftExtended -^ rightExtended
  distMat <- R.sumP absDiff
  R.computeUnboxedP $ R.map (<= 3) distMat

adjacencyMatToGraph :: Array U DIM2 Bool -> Graph
adjacencyMatToGraph adjMat = do
  let Z :. numPoints :. _ = R.extent adjMat
      rowVec :: Int -> VU.Vector Bool
      rowVec i = R.toUnboxed $ R.computeS $ R.slice adjMat (Z :. i :. All)
      nodeAdjList :: Int -> [Int]
      nodeAdjList i = VU.ifoldr (\j adjacent rest -> if adjacent then j : rest else rest) [] $ rowVec i
   in buildG (0, numPoints - 1) [(i, j) | i <- [0 .. numPoints - 1], j <- nodeAdjList i]

main :: IO ()
main = do
    testPoints <- parseOrDie coordParser <$> TIO.readFile "aoc18_day25_test"
    let computeConstellations p = length . components . adjacencyMatToGraph <$> adjacencyMatrix p
    testNumConstellations <- computeConstellations testPoints
    assert (testNumConstellations == 8) $ do
        points <- parseOrDie coordParser <$> TIO.readFile "aoc18_day25"
        numConstellations <- computeConstellations points
        putStrLn $ "Solution to question 1 is: " ++ show numConstellations
