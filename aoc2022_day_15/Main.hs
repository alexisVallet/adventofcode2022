module Main where

import Data.Array.Accelerate (Acc, DIM2, Elt, Exp, Scalar, Vector, Z (..), constant, (:.) (..))
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.Control.Lens (liftLens)
import Data.Array.Accelerate.LLVM.PTX (run, runN)
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

beaconParser :: Parsec Void Text [((Int, Int), (Int, Int))]
beaconParser = some $ do
  string "Sensor at "
  let parseCoords = (,) <$> (string "x=" >> signed space decimal) <*> (string ", y=" >> signed space decimal)
  sensorCoords <- parseCoords
  string ": closest beacon is at "
  beaconCoords <- parseCoords
  newline
  return (sensorCoords, beaconCoords)

reorder :: (Int, Int) -> (Int, Int)
reorder (i, j) = if i <= j then (i, j) else (j, i)

noBeaconPoints :: Int -> (Int, Int) -> (Int, Int) -> Set (Int, Int)
noBeaconPoints y (sx, sy) (bx, by) =
  let sensorRadius = abs (bx - sx) + abs (by - sy)
      minDist = abs (y - sy)
      c = sensorRadius - minDist
      (start, end) = reorder (sx - c, sx + c)
   in if minDist > sensorRadius then mempty else Set.fromList [(x', y) | x' <- [start .. end]]

numNoBeaconPoints :: [((Int, Int), (Int, Int))] -> Int -> Int
numNoBeaconPoints beaconSensorPairs y =
  length $ Set.difference (Set.unions $ fmap (uncurry $ noBeaconPoints y) beaconSensorPairs) (Set.fromList $ snd <$> beaconSensorPairs)

type instance Index (Exp (a, a2)) = Int

type instance IxValue (Exp (a, a2)) = Exp a

instance (Elt a, a ~ a2) => Ixed (Exp (a, a2)) where
  ix p = liftLens (elementOf each p :: Traversal' (Exp (a, a2)) (IxValue (Exp (a, a2))))

indexArrElem :: Exp DIM2 -> Exp DIM2 -> Exp (Int, Int)
indexArrElem (A.unindex2 -> startPoint) (A.unindex2 -> offset) =
  let x = (startPoint ^?! ix 0) + (offset ^?! ix 0)
      y = (startPoint ^?! ix 1) + (offset ^?! ix 1)
   in A.lift (x, y)

intersectsAny :: [((Int, Int), (Int, Int))] -> Exp (Int, Int) -> Exp Bool
intersectsAny beaconSensorPairs point =
  let x = point ^?! ix 0
      y = point ^?! ix 1
      accIntersects =
        \((sx, sy), (bx, by)) didIntersect ->
          didIntersect A.|| A.abs (x - constant sx) + A.abs (y - constant sy) A.<= constant (abs (bx - sx) + abs (by - sy))
   in A.not $ foldr accIntersects (constant False) beaconSensorPairs

findDistressBeaconInBatch :: [((Int, Int), (Int, Int))] -> (Int, Int) -> Acc (Scalar DIM2) -> Acc (Vector (Int, Int))
findDistressBeaconInBatch beaconSensorPairs (bw, bh) startPoint =
  let shape = constant (Z :. bw :. bh)
      indexArr = A.generate shape (indexArrElem $ A.the startPoint)
      intersectsArr = A.map (intersectsAny beaconSensorPairs) indexArr
   in A.afst $ A.compact (A.flatten intersectsArr) (A.flatten indexArr)

findDistressBeacon :: [((Int, Int), (Int, Int))] -> (Int, Int) -> IO (Maybe (Vector (Int, Int)))
findDistressBeacon beaconSensorPairs batchSize@(bw, bh) = do
  let findFunctionR = runN $ findDistressBeaconInBatch beaconSensorPairs batchSize
      maxX = 4000000
      maxY = 4000000
      allStartPoints = zip [0 ..] [(startX, startY) | startX <- [0, bw .. maxX], startY <- [0, bh .. maxY]]
      totalNumIt = length allStartPoints
      doFind startPoints =
        case startPoints of
          [] -> return Nothing
          ((i, (startX, startY)) : rest) -> do
            let batchOutputs = findFunctionR (A.fromList Z [Z :. startX :. startY])
            when (i `mod` 100 == 0) $ do
              putStrLn $ "Iteration " ++ show i ++ " out of " ++ show totalNumIt
            if A.arraySize batchOutputs > 0
              then return $ Just batchOutputs
              else doFind rest
  doFind allStartPoints

main :: IO ()
main = do
  testBeaconSensorPairs <- parseOrDie beaconParser <$> TIO.readFile "aoc22_day15_test"
  let testActual = numNoBeaconPoints testBeaconSensorPairs 10
  print testActual
  assert (testActual == 26) $ do
    beaconSensorPairs <- parseOrDie beaconParser <$> TIO.readFile "aoc22_day15_input"
    putStrLn $ "Question 1 answer is: " ++ show (numNoBeaconPoints beaconSensorPairs 2000000)
    mCoords <- findDistressBeacon beaconSensorPairs (4096, 4096)
    putStrLn $ "Question 2 answer is: " ++ show mCoords
