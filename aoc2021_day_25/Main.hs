module Main where

import Control.Exception
import Control.Monad
import Data.Array.Repa (Array, DIM2, Z (..), ix2, (:.) (..))
import Data.Array.Repa qualified as R
import Data.Array.Repa.Eval qualified as R
import Data.Array.Repa.Repr.Vector (V)
import Data.Array.Repa.Repr.Vector qualified as R
import Data.List.Split (chunksOf)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void
import ParseUtils
import Text.Megaparsec
import Text.Megaparsec.Char

data Tile = R | D | E
  deriving (Eq, Ord, Show)

charToTile :: Char -> Tile
charToTile 'v' = D
charToTile '>' = R
charToTile '.' = E
charToTile _ = error "unexpected character!"

tileToChar :: Tile -> Char
tileToChar D = 'v'
tileToChar R = '>'
tileToChar E = '.'

parseSeaFloor :: Parsec Void Text (Array V DIM2 Tile)
parseSeaFloor = do
  tileLines <- many $ do
    line <- some $ charToTile <$> oneOf ['v', '>', '.']
    eof <|> do
      _ <- newline
      return ()
    return line
  when (null tileLines) $ fail "No lines parsed!"
  let height = length tileLines
      width = length $ head tileLines
  return $ R.fromList (Z :. height :. width) (concat tileLines)

transitionRight :: (Tile, Tile, Tile) -> Tile
transitionRight pattern =
  case pattern of
    (R, E, _) -> R
    (_, R, E) -> E
    (_, c, _) -> c

transitionDown :: (Tile, Tile, Tile) -> Tile
transitionDown pattern =
  case pattern of
    (D, E, _) -> D
    (_, D, E) -> E
    (_, c, _) -> c

mirrorCoord :: Int -> Int -> Int
mirrorCoord size x =
  let posX = if x < 0 then size + x else x
   in posX `mod` size

mirror2D :: DIM2 -> DIM2 -> DIM2
mirror2D (Z :. h :. w) (Z :. y :. x) = Z :. mirrorCoord h y :. mirrorCoord w x

updateTile :: DIM2 -> Bool -> ((Tile, Tile, Tile) -> Tile) -> (DIM2 -> Tile) -> DIM2 -> Tile
updateTile size isHorizontal transition indexFn pos@(Z :. y :. x) =
  let left = indexFn $ mirror2D size $ if isHorizontal then ix2 y (x - 1) else ix2 (y - 1) x
      right = indexFn $ mirror2D size $ if isHorizontal then ix2 y (x + 1) else ix2 (y + 1) x
   in transition (left, indexFn pos, right)

doTransition :: (Monad m) => Bool -> ((Tile, Tile, Tile) -> Tile) -> Array V DIM2 Tile -> m (Array V DIM2 Tile)
doTransition isHorizontal transition input =
  R.computeVectorP $ R.traverse input id (updateTile (R.extent input) isHorizontal transition)

stepSeaFloor :: (Monad m) => Array V DIM2 Tile -> m (Array V DIM2 Tile)
stepSeaFloor curSeaFloor = do
  tmpSeaFloor <- doTransition True transitionRight curSeaFloor
  doTransition False transitionDown tmpSeaFloor

showSeaFloor :: Array V DIM2 Tile -> String
showSeaFloor seaFloorArr =
  let Z :. _ :. width = R.extent seaFloorArr
      seaFloorList = chunksOf width $ R.toList seaFloorArr
      showSeaFloorLine :: [Tile] -> String
      showSeaFloorLine = foldr (\t curLineRep -> tileToChar t : curLineRep) ""
   in foldr (\line curRep -> showSeaFloorLine line ++ "\n" ++ curRep) "" seaFloorList

iterateSeaFloor :: Int -> Array V DIM2 Tile -> IO ()
iterateSeaFloor step seaFloorArr = do
  newSeaFloorArr <- stepSeaFloor seaFloorArr
  if seaFloorArr == newSeaFloorArr
    then do
      putStrLn $ "Answer to question 1 is: " ++ show step
      putStrLn "Final sea floor:"
      putStrLn $ showSeaFloor newSeaFloorArr
    else do
      iterateSeaFloor (step + 1) newSeaFloorArr

main :: IO ()
main = do
  let readAndParse fname = parseOrDie parseSeaFloor <$> TIO.readFile fname
  testIn <- readAndParse "aoc21_day25_test_in"
  testExpectedOut <- readAndParse "aoc21_day25_test_out"
  putStrLn "Input: "
  putStrLn $ showSeaFloor testIn
  testActualOut <- stepSeaFloor testIn
  putStrLn "Actual: "
  putStrLn $ showSeaFloor testActualOut
  putStrLn "Expected: "
  putStrLn $ showSeaFloor testExpectedOut
  assert (testExpectedOut == testActualOut) $ do
    inputSeaFloor <- readAndParse "aoc21_day25"
    iterateSeaFloor 1 inputSeaFloor
