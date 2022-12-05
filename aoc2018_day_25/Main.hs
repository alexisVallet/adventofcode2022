module Main where

import Data.Graph
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Void
import ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char ( char, newline )
import Text.Megaparsec.Char.Lexer
import Data.Text (Text)
import Data.Text.IO qualified as TIO

import Data.Array.Repa (Array, DIM2, Z (..), ix2, ix3, (:.) (..), U, D, All(..), (-^))
import Data.Array.Repa qualified as R

coordParser :: Parsec Void Text (Array U DIM2 Int)
coordParser = do
    coordList <- many $ do
        n1 <- decimal
        _ <- char ','
        n2 <- decimal
        _ <- char ','
        n3 <- decimal
        _ <- char ','
        n4 <- decimal
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


adjacencyMatToGraph :: (Monad m) => Array U DIM2 Bool -> m Graph
adjacencyMatToGraph adjMat = do
    let Z :. numPoints :. _ = R.extent adjMat
        rowVec :: Int -> VU.Vector Bool
        rowVec i = R.toUnboxed $ R.computeS $ R.slice adjMat (Z :. i :. All)
        nodeAdjList :: Int -> [[Int]]
        nodeAdjList i = VU.toList $ VU.ifoldr (\j adjacent rest -> if adjacent then j:rest else rest) [] $ rowVec i
    return $ undefined



main :: IO ()
main = undefined