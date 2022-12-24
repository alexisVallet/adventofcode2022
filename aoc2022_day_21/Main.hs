module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import Imports hiding (Vertex)
import ParseUtils

type Vertex = String

data Node = Constant Int | BinOp (Int -> Int -> Int) Vertex Vertex

type ComputeGraph = Map Vertex Node

parseName = replicateM 4 anySingle

parseOp :: Parsec Void Text (Int -> Int -> Int)
parseOp = do
    opChar <- oneOf ("+-*/" :: [Char])
    return $ case opChar of
        '+' -> (+)
        '-' -> (-)
        '*' -> (*)
        '/' -> div
        _ -> error "This should never happen!"

parseNode :: Parsec Void Text Node
parseNode = Constant <$> decimal <|> do
    name1 <- parseName
    void hspace
    op <- parseOp
    void hspace
    BinOp op name1 <$> parseName

parseComputeGraph :: Parsec Void Text ComputeGraph
parseComputeGraph = fmap Map.fromList $ some $ do
    name <- parseName
    void $ char ':' >> hspace
    node <- parseNode
    newline
    return (name, node)

computeNode :: String -> ComputeGraph -> Int
computeNode name graph =
    case Map.lookup name graph of
        Nothing -> error $ "Invalid name " ++ name
        Just (Constant i) -> i
        Just (BinOp op l r) -> op (computeNode l graph) (computeNode r graph)

main :: IO ()
main = do
    testGraph <- parseOrDie parseComputeGraph <$> TIO.readFile "aoc22_day21_test"
    let actualRootVal = computeNode "root" testGraph
    print actualRootVal
    assert (actualRootVal == 152) $ return ()
    graph <- parseOrDie parseComputeGraph <$> TIO.readFile "aoc22_day21_input"
    putStrLn $ "Question 1 answer is: " ++ show (computeNode "root" graph)
