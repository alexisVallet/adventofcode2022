module Main (main) where

import Data.Array.Accelerate (Exp)
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.Interpreter qualified as AI
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Imports hiding (Vertex)
import ParseUtils
import Z3.Monad hiding (assert)
import Z3.Monad qualified as Z3

type Vertex = String

data Op = Add | Sub | Mul | Div
  deriving (Eq, Ord)

opToExp :: Op -> (Exp Int -> Exp Int -> Exp Int)
opToExp Add = (+)
opToExp Sub = (-)
opToExp Mul = (*)
opToExp Div = div

opToZ3 :: MonadZ3 z3 => Op -> AST -> AST -> z3 AST
opToZ3 Add x y = mkAdd [x, y]
opToZ3 Sub x y = mkSub [x, y]
opToZ3 Mul x y = mkMul [x, y]
opToZ3 Div x y = mkDiv x y

data Node
  = Constant Int
  | BinOp Op Vertex Vertex

type ComputeGraph = Map Vertex Node

parseName = replicateM 4 anySingle

parseOp :: Parsec Void Text Op
parseOp = do
  opChar <- oneOf ("+-*/" :: [Char])
  return $ case opChar of
    '+' -> Add
    '-' -> Sub
    '*' -> Mul
    '/' -> Div
    _ -> error "This should never happen!"

parseNode :: Parsec Void Text Node
parseNode =
  Constant <$> decimal <|> do
    name1 <- parseName
    void hspace
    op <- parseOp
    void hspace
    BinOp op name1 <$> parseName

parseComputeGraph :: Parsec Void Text ComputeGraph
parseComputeGraph = fmap Map.fromList $
  some $ do
    name <- parseName
    void $ char ':' >> hspace
    node <- parseNode
    newline
    return (name, node)

computeNodeExp :: String -> ComputeGraph -> Exp Int
computeNodeExp name graph =
  case Map.lookup name graph of
    Nothing -> error $ "Invalid name " ++ name
    Just (Constant i) -> A.constant i
    Just (BinOp op l r) -> opToExp op (computeNodeExp l graph) (computeNodeExp r graph)

computeNode :: String -> ComputeGraph -> Int
computeNode name graph =
  let exp = computeNodeExp name graph
      acc = A.unit exp
   in A.indexArray (AI.run acc) A.Z

computeGraphToZ3Q2 :: ComputeGraph -> Z3 (Maybe Integer)
computeGraphToZ3Q2 graph = do
  let (BinOp _ eqL eqR) = graph ^?! ix "root"
  humnNode <- mkFreshIntVar "humn"
  let nodeToZ3 name =
        case name of
          "humn" -> return humnNode
          _ -> case graph ^?! ix name of
            BinOp op l r -> do
              lAst <- nodeToZ3 l
              rAst <- nodeToZ3 r
              opToZ3 op lAst rAst
            Constant i -> mkInteger $ fromIntegral i
  eqLast <- nodeToZ3 eqL
  eqRast <- nodeToZ3 eqR
  eqZ3 <- mkEq eqLast eqRast
  Z3.assert eqZ3
  fmap (join . snd) $ withModel $ \m -> evalInt m humnNode

main :: IO ()
main = do
  testGraph <- parseOrDie parseComputeGraph <$> TIO.readFile "aoc22_day21_test"
  let actualRootVal = computeNode "root" testGraph
  print actualRootVal
  assert (actualRootVal == 152) $ return ()
  graph <- parseOrDie parseComputeGraph <$> TIO.readFile "aoc22_day21_input"
  putStrLn $ "Question 1 answer is: " ++ show (computeNode "root" graph)
  let graphScript = computeGraphToZ3Q2 graph
  maybeSolution <- evalZ3 graphScript
  case maybeSolution of
    Nothing -> putStrLn "No solution found for Q2!"
    Just sol -> putStrLn $ "Question 2 answer is: " ++ show sol
