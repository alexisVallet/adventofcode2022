module Main (main) where

import Data.Array.Repa as R hiding ((++))
import Data.Array.Repa.Repr.Vector as R
import Data.Graph.Inductive hiding ((&))
import Data.Graph.Inductive qualified as G
import Data.IntMap.Strict qualified as IMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Imports hiding (Empty)
import ParseUtils

data Face = Face
  { tiles :: Array V DIM2 ((Int, Int), Bool),
    corners :: Map CubeVertex DIM2,
    cubeEdges :: Map (Int, Int) (CubeVertex, CubeVertex)
  }
  deriving (Generic)

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
      posArray = fromFunction (extent boardArray) (\(Z :. i :. j) -> (i + 1, j + 1))
      -- We associate to every tile its location in the original board so we can find it later
      boardArray' = R.zipWith (,) posArray boardArray
      faceMap =
        IMap.fromList $
          zip
            [0 ..]
            [ R.computeVectorS $ R.map (second fromJust) $ extract (ix2 i j) (ix2 size size) boardArray'
              | i <- [0, size .. numRows -1],
                j <- [0, size .. numCols -1],
                isJust (boardArray ! ix2 i j)
            ]
      cornerLocs = [ix2 0 0, ix2 0 (size -1), ix2 (size -1) 0, ix2 (size -1) (size -1)]
  -- Parsing corner specifications in the extended format
  faceCorners <- fmap IMap.fromList $
    some $ do
      faceIdx <- decimal
      void $ char ':'
      corners@[a, b, c, d] <- some $ oneOf ['A' .. 'H']
      void newline
      return (faceIdx, (Map.fromList $ zip corners cornerLocs, Map.fromList [(up, (a, b)), (left, (a, c)), (right, (b, d)), (down, (c, d))]))
  -- Putting it all together
  return $
    IMap.fromList
      [ (i, Face tiles corners cubeEdges)
        | ((i, tiles), (corners, cubeEdges)) <- zip (IMap.assocs faceMap) (IMap.elems faceCorners)
      ]

-- Represent the board by a graph, with edges labelled with
-- their direction, and nodes labelled with their position in
-- the original, flat board.
type Board = Gr (Int, Int) (Int, Int)

up, down, left, right :: (Int, Int)
up = (-1, 0)
down = (1, 0)
left = (0, -1)
right = (0, 1)

facesToBoard :: IntMap Face -> Board
facesToBoard faces =
  let -- We start by associating a unique graph node key to every location in each face
      faceToNodeKeys (faceIdx, Face {tiles = tiles}) =
        let Z :. h :. w = extent tiles
         in [(faceIdx, ix2 i j) | i <- [0 .. h -1], j <- [0 .. w -1], not . snd $ tiles ! ix2 i j]
      nodeMap = Map.fromList $ zip (concatMap faceToNodeKeys $ IMap.assocs faces) [0 ..]
      -- We then compute the "inner" edges between graph nodes within each face
      faceToInnerEdges (faceIdx, Face {tiles = tiles}) =
        concat $
          R.toList $
            R.computeVectorS $
              R.traverse tiles id $ \_ pos@(Z :. i :. j) ->
                case Map.lookup (faceIdx, pos) nodeMap of
                  Nothing -> []
                  Just srcNode ->
                    (\(dir, dstNode) -> (srcNode, dstNode, dir))
                      <$> mapMaybe
                        ( \dir@(di, dj) ->
                            let pos' = ix2 (i + di) (j + dj)
                                mNode = Map.lookup (faceIdx, pos') nodeMap
                             in mNode <&> (dir,)
                        )
                        [up, down, left, right]
      innerEdges = concatMap faceToInnerEdges $ IMap.assocs faces
      -- Then we compute the "outer" graph edges linking faces together, by
      -- considering common cube edges between all pairs of faces, and adding
      -- graph edges node by node along those common cube edges.
      iterateCoords face (c1, c2) =
        let (Z :. i1 :. j1) = face ^?! #corners . ix c1
            (Z :. i2 :. j2) = face ^?! #corners . ix c2
         in if i1 == i2
              then [Z :. i1 :. j | j <- [j1, j1 + signum (j2 - j1) .. j2]]
              else [Z :. i :. j1 | i <- [i1, i1 + signum (i2 - i1) .. i2]]
      outerEdges =
        concatMap
          ( \((i1, f1), (i2, f2)) ->
              let commonEdges =
                    [ (dir, e1, e2)
                      | (dir, e1) <- fmap (second reorder) $ Map.assocs $ f1 ^. #cubeEdges,
                        e2 <- fmap reorder $ Map.elems $ f2 ^. #cubeEdges,
                        e1 == e2
                    ]
               in concatMap
                    ( \(dir, e1, e2) ->
                        mapMaybe
                          ( \(pos1, pos2) -> do
                              n1 <- Map.lookup (i1, pos1) nodeMap
                              n2 <- Map.lookup (i2, pos2) nodeMap
                              return (n1, n2, dir)
                          )
                          $ zip (iterateCoords f1 e1) (iterateCoords f2 e2)
                    )
                    commonEdges
          )
          [(f1, f2) | f1 <- IMap.assocs faces, f2 <- IMap.assocs faces, fst f1 /= fst f2]
      edges = innerEdges ++ outerEdges
      nodes = [(n, fst $ (faces ^?! ix faceIdx . #tiles) ! tileIdx) | ((faceIdx, tileIdx), n) <- Map.assocs nodeMap]
   in mkGraph nodes edges

-- These datatypes are in common with question 1.
data Turn = Turn
  { side :: Either () (),
    next :: Move
  }
  deriving (Generic, Eq, Ord, Show)

data Move = Move
  { amount :: Int,
    next :: Maybe Turn
  }
  deriving (Generic, Eq, Ord, Show)

moveParser :: Parsec Void Text Move
moveParser = do
  amount <- decimal
  turn <- turnParser
  return $
    Move
      { amount = amount,
        next = turn
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
      return $
        Just $
          Turn
            { side = side,
              next = move
            }

data SimState = SimState
  { board :: Board,
    curNode :: Node,
    curDir :: (Int, Int)
  }
  deriving (Generic, Eq, Show)

type Sim = State SimState

fullParser :: Int -> Parsec Void Text (Board, Move)
fullParser size = do
  faces <- facesParser size
  void newline
  move <- moveParser
  return (facesToBoard faces, move)

initState :: (Int, Int) -> Board -> SimState
initState startPos board =
  let startNode = fst $ fromJust $ find ((== startPos) . snd) $ labNodes board
   in SimState
        { board = board,
          curNode = startNode,
          curDir = right
        }

performMove :: Move -> Sim ()
performMove (Move amount mNextTurn) = do
  if amount == 0
    then performTurn mNextTurn
    else do
      curNode' <- use #curNode
      dir <- use #curDir
      board <- use #board
      let sucMap = Map.fromList $ (\(a, b) -> (b, a)) <$> lsuc board curNode'
      case Map.lookup dir sucMap of
        Nothing -> performTurn mNextTurn
        Just newNode -> do
          #curNode .= newNode
          -- To handle direction properly when we wrap to
          -- another plane, we need to update the current direction
          -- to be the opposite of the one of the edge from the current
          -- node to the previous node.
          let (dx, dy) = fromJust $ lookup curNode' $ lsuc board newNode
          #curDir .= (- dx, - dy)
          performMove (Move (amount - 1) mNextTurn)

performTurn :: Maybe Turn -> Sim ()
performTurn Nothing = return ()
performTurn (Just (Turn side nextMove)) = do
  #curDir %= \(dy, dx) -> case side of
    Left _ -> (- dx, dy)
    Right _ -> (dx, - dy)
  performMove nextMove

password :: (Int, Int) -> Board -> Move -> Int
password startPos board move =
  let finalState = execState (performMove move) (initState startPos board)
      (finalRow, finalCol) = fromJust $ lab (finalState ^. #board) (finalState ^. #curNode)
      facing = case finalState ^. #curDir of
        (0, 1) -> 0
        (1, 0) -> 1
        (0, -1) -> 2
        (-1, 0) -> 3
        _ -> error $ "Invalid direction " ++ show (finalState ^. #curDir)
   in 1000 * finalRow + 4 * finalCol + facing

main :: IO ()
main = do
  (testBoard, testMove) <- parseOrDie (fullParser 4) <$> TIO.readFile "aoc22_day22_test_modified"
  let actualPassword = password (1, 9) testBoard testMove
  assert (actualPassword == 5031) $ return ()
  (board, move) <- parseOrDie (fullParser 50) <$> TIO.readFile "aoc22_day22_input_modified"
  putStrLn $ "Question 2 answer is: " ++ show (password (1, 51) board move)
