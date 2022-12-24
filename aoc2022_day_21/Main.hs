module Main (main) where

import Imports
import ParseUtils

type Vertex = Int

data Node = Constant Int | BinOp

type ComputeGraph = IntMap Node

parseComputeGraph :: Parsec Void Text Compu

main :: IO ()
main = undefined
