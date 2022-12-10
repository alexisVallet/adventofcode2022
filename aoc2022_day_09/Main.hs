module Main where

import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import GHC.Int
import Imports
import Numeric.LinearAlgebra hiding (find)
import ParseUtils
import Prelude hiding ((<>))

parseDir :: Char -> Vector Z
parseDir c =
  fromList $ case c of
    'R' -> [1, 0]
    'U' -> [0, -1]
    'L' -> [-1, 0]
    'D' -> [0, 1]
    _ -> error $ "Invalid character " ++ show c

motionSeriesParser :: Parsec Void Text [Vector Z]
motionSeriesParser = fmap concat $
  some $ do
    dirVec <- parseDir <$> oneOf ("RULD" :: [Char])
    hspace
    numSteps <- decimal
    void newline
    return $ replicate numSteps dirVec

data SimState = SimState
  { h :: Vector Z,
    ts :: V.Vector (Vector Z),
    hPosHist :: [Vector Z],
    tsPosHist :: V.Vector [Vector Z]
  }
  deriving (Generic, Show)

initState :: Int -> SimState
initState numKnots =
  let origin = fromList [0, 0]
   in SimState
        { h = origin,
          ts = V.fromList $ replicate numKnots origin,
          hPosHist = [origin],
          tsPosHist = V.fromList $ replicate numKnots [origin]
        }

rotMat :: Matrix Z
rotMat =
  fromLists
    [ [0, -1],
      [1, 0]
    ]

flipLRMat :: Matrix Z
flipLRMat =
  fromLists
    [ [-1, 0],
      [0, 1]
    ]

{-
Up to 90 degree rotation around H and left/right flips around H, there are basically
only 3 possible cases:
H           H
     ->     T
T

H           H
     ->     T
 T

H         H
     ->    T
  T

So we find the transform bringing us back to this case by brute force
(only 8 cases), then transform back the results by taking the
inverse. Because those are all orthogonal matrices, we find the inverse
by taking the transpose.
-}
tryUpdateTPosCanonical :: Vector Z -> Maybe (Vector Z)
tryUpdateTPosCanonical relTPos =
  case toList relTPos of
    [0, 2] -> Just $ fromList [0, 1]
    [1, 2] -> Just $ fromList [0, 1]
    [2, 2] -> Just $ fromList [1, 1]
    _ -> Nothing

updateTPos :: Vector Z -> Vector Z -> Vector Z
updateTPos hPos tPos =
  let relTPos = tPos - hPos
      possibleRotations = take 4 $ iterate (<> rotMat) $ ident 2
      possibleTransforms = fmap (<> flipLRMat) possibleRotations ++ possibleRotations
      matchedTransformAndRes =
        fmap (second fromJust) $
          find (isJust . snd) $
            fmap (\m -> (m, tryUpdateTPosCanonical $ m #> relTPos)) possibleTransforms
   in case matchedTransformAndRes of
        Nothing -> error $ "Did not find a matching case and transform for: hPos=" ++ show hPos ++ ", tPos=" ++ show tPos
        Just (transMat, relTPos) ->
          let backRelHPos = tr transMat #> relTPos
              outTPos = hPos + backRelHPos
           in outTPos

dist :: Vector Z -> Vector Z -> Int64
dist x y = maximum $ toList $ abs (x - y)

simStep :: Vector Z -> State SimState ()
simStep movement = do
  hPos <- #h <+= movement
  knotPositions <- V.toList <$> use #ts
  let numKnots = length knotPositions
      nextKnotPos = hPos : knotPositions
  forM_ (zip [0 .. numKnots -1] nextKnotPos) $ \(curKnotIdx, hPos) -> do
    tPos <- use $ #ts . ix curKnotIdx
    let distToTarget = dist hPos tPos
    when (distToTarget > 1) $ do
      #ts . ix curKnotIdx %= updateTPos hPos
    newTPos <- use $ #ts . ix curKnotIdx
    assert (dist hPos newTPos <= 1) $ do
      #tsPosHist . ix curKnotIdx %= (newTPos :)
      #hPosHist %= (hPos :)

simMoves :: [Vector Z] -> State SimState ()
simMoves moves = forM_ moves simStep

runSimMoves :: Int -> [Vector Z] -> SimState
runSimMoves numKnots moves = execState (simMoves moves) $ initState numKnots

countTailVisited :: Int -> [Vector Z] -> Int
countTailVisited numKnots moves =
  let allKnotHist = tsPosHist $ runSimMoves numKnots moves
   in length $ Set.fromList $ allKnotHist ^. ix (numKnots - 1)

main :: IO ()
main = do
  testMoves <- parseOrDie motionSeriesParser <$> TIO.readFile "aoc22_day09_test"
  let actualNumVisited1 = countTailVisited 1 testMoves
      actualNumVisited2 = countTailVisited 9 testMoves
  assert (actualNumVisited1 == 13 && actualNumVisited2 == 1) $ do
    moves <- parseOrDie motionSeriesParser <$> TIO.readFile "aoc22_day09_input"
    putStrLn $ "Question 1 answer is: " ++ show (countTailVisited 1 moves)
    putStrLn $ "Question 2 answer is: " ++ show (countTailVisited 9 moves)
