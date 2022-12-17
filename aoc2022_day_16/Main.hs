module Main where

import Control.Parallel.Strategies
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Coroutine.Nested
import Data.Functor.Sum
import Data.Graph.Inductive
import Data.Graph.Inductive qualified as G
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils


data NodeLabel = NodeLabel {
    flowRate :: Int,
    name :: (Char, Char)
} deriving (Generic, Show)

data TunnelGraph = TunnelGraph {
    graph :: Gr NodeLabel Int,
    startNode :: Node
} deriving (Generic, Show)

parseGraph :: Parsec Void Text (TunnelGraph, Map (Char, Char) Node)
parseGraph = do
    nodeAndNeighbors <- some $ do
        void $ string "Valve "
        let parseName = (,) <$> upperChar <*> upperChar
        name <- parseName
        void $ string " has flow rate="
        flowRate <- decimal
        void $ string "; tunnels lead to valves " <|> string "; tunnel leads to valve "
        neighbors <- sepBy parseName (string ", ")
        void newline
        return (NodeLabel {flowRate=flowRate, name=name}, neighbors)
    let accNodes (nodeLabel, _) (curGraph, curNode, nameToNode) =
            (insNode (curNode, nodeLabel) curGraph, curNode + 1, Map.insert (name nodeLabel) curNode nameToNode)
        (nodeGraph, _, nameToNode) = foldr accNodes (G.empty, 0, mempty) nodeAndNeighbors
        accEdges (nodeLabel, neighbors) =
            insEdges [(nameToNode Map.! name nodeLabel, nameToNode Map.! n, 1) | n <- neighbors]
        graph = foldr accEdges nodeGraph nodeAndNeighbors
    return (TunnelGraph graph $ nameToNode Map.! ('A', 'A'), nameToNode)

nonZeroNodes :: TunnelGraph -> [Node]
nonZeroNodes (TunnelGraph graph startNode) = fmap fst (filter (\(_, n) -> flowRate n > 0) $ labNodes graph)

allPairsShortestPath :: TunnelGraph -> [Node] -> Map (Node, Node) Int
allPairsShortestPath (TunnelGraph graph startNode) nodes =
    let nodes' = startNode : nodes
    in Map.fromList [((n1, n2), fromJust $ spLength n1 n2 graph) | n1 <- nodes', n2 <- nodes', n1 /= n2]

data SimState = SimState {
    tunnel :: TunnelGraph,
    spLengthMap :: Map (Node, Node) Int,
    curNode :: Int,
    curFlowRate :: Int,
    unexplored :: Set Node
} deriving (Generic, Show)

initState :: TunnelGraph -> Map (Node, Node) Int -> [Node] -> SimState
initState tunnel spLengthMap nonZeroNodes = SimState {
    tunnel=tunnel, spLengthMap=spLengthMap, curNode=startNode tunnel,
    curFlowRate=0, unexplored=Set.fromList nonZeroNodes}

instance (Functor s', MonadState s m) => MonadState s (Coroutine s' m) where
    state f = lift (state f)

-- Coroutine requesting the next node to move to, or Nothing to stay at the
-- current node and bring an early end to the simulation.
type SimMonad = Coroutine (
    Request 
    (Set Node)  -- set of nodes to pick the next node to explore 
    (Maybe Node)  -- chosen node, Nothing to stop here until the end of time
    ) (State SimState)

type PlayerMonad = 
    Coroutine (
        Sum 
        (Request (Set Node) (Maybe Node)) -- Same as SimMonad
        (Request 
        (Node, Int) -- Next node the player intends to explore and current time remaining
        (Set Node)  -- Set of unexplored nodes after the other player caught up
        )
    ) (State SimState)

scheduleSingle :: PlayerMonad a -> SimMonad a
scheduleSingle = pogoStickNested handleRight
    where 
        handleRight (Request (curNode, _) nextStep) = do
            -- Single player simply updates the set of unexplored nodes
            -- and return it, since no synchronization is necessary.
            unexplored <- #unexplored <%= Set.delete curNode
            nextStep unexplored

singlePressureRelease :: PlayerMonad Int
singlePressureRelease = do
    doSimulate 30
    where
        doSimulate 0 = return 0
        doSimulate minutesRemaining = do
            curNode <- use #curNode
            unexplored <- mapSuspension InR $ request (curNode, minutesRemaining)
            valveFlowRate <- flowRate . fromJust . flip lab curNode <$> use (#tunnel . #graph)
            curFlowRate <- use #curFlowRate
            let (nextFlowRate, minutesThere) =
                    if valveFlowRate == 0 then (curFlowRate, 0) else
                        (curFlowRate + valveFlowRate, 1)
            mNextNode <- mapSuspension InL $ request unexplored
            let terminate = return $ curFlowRate + nextFlowRate * (minutesRemaining - 1)
            case mNextNode of
                Nothing -> terminate
                Just nextNode -> do
                    -- traceM $ "curNode is " ++ show curNode ++ ", requested one of " ++ show unexplored ++ ", got " ++ show nextNode
                    spLengthMap <- use #spLengthMap
                    let minutesMoving = spLengthMap Map.! (curNode, nextNode)
                        canMove = minutesRemaining - minutesThere - minutesMoving >= 0
                        actuallyMovingMinutes = if canMove then minutesMoving else 0
                        newMinutesRemaining = minutesRemaining - minutesThere - actuallyMovingMinutes
                    if not canMove then terminate else do
                        #curNode .= nextNode
                        #curFlowRate .= nextFlowRate
                        rest <- doSimulate newMinutesRemaining
                        return $ minutesThere * curFlowRate + actuallyMovingMinutes * nextFlowRate + rest

simulatePressureRelease :: (PlayerMonad Int -> SimMonad b) -> SimMonad b
simulatePressureRelease scheduler = scheduler singlePressureRelease

runWithPath :: [Node] -> SimMonad a -> State SimState a
runWithPath path sim = do
    stepOut <- resume sim
    case stepOut of
        Right result -> return result
        Left (Request _ nextStep) -> do
            case path of
                [] -> runWithPath [] (nextStep Nothing)
                (n:ns) -> runWithPath ns (nextStep $ Just n)


allValidPathsFrom :: Map (Int, Int) Int -> Int -> Node -> Set Node -> [[Node]]
allValidPathsFrom spLengthMap minutesRemaining curNode nodesRemaining =
    let accessibleNodes =
            filter ((>= 0) . snd)
            $ [(n, minutesRemaining - 1 - (spLengthMap Map.! (curNode, n))) | n <- Set.toList nodesRemaining]
    in if null accessibleNodes then [[curNode]]
        else concatMap
                (\(n, newMRemain) -> (curNode :) <$> allValidPathsFrom spLengthMap newMRemain n (Set.delete n nodesRemaining))
                accessibleNodes

validPaths :: Map (Int, Int) Int -> Int -> [Node] -> [[Node]]
validPaths spLengthMap startNode otherNodes =
    let nodeSet = Set.fromList otherNodes
    in concatMap (\(n, newRemain) -> allValidPathsFrom spLengthMap newRemain n (Set.delete n nodeSet))
            $ filter ((>= 0) . snd)
            $ [(n, 30 - (spLengthMap Map.! (startNode, n))) | n <- otherNodes]


stepSim :: SimState -> SimMonad a -> (Either (Request (Set Node) (Maybe Node) (SimMonad a)) a, SimState)
stepSim curState sim = flip runState curState $ resume sim

searchAllPaths :: SimState -> SimMonad a -> [(a, SimState)]
searchAllPaths curState sim =
    let (stepOut, nextState) = stepSim curState sim
    in case stepOut of
        Left (Request possibleNodes nextStep) ->
            let moves = Set.insert Nothing $ Set.map Just possibleNodes
            in concatMap (searchAllPaths nextState . nextStep) moves
        Right result -> [(result, nextState)]

searchMaxPressureRelease :: TunnelGraph -> Int
searchMaxPressureRelease tunnel =
    let nodes = nonZeroNodes tunnel
        spLengthMap = allPairsShortestPath tunnel nodes
    in foldl (\(force -> !curMax) pressureVal -> max curMax pressureVal) 0
        $ fst <$> searchAllPaths (initState tunnel spLengthMap nodes) (simulatePressureRelease scheduleSingle)

maxPressureRelease :: TunnelGraph -> IO Int
maxPressureRelease tunnelGraph = do
    let nodes = nonZeroNodes tunnelGraph
        spLengthMap = allPairsShortestPath tunnelGraph nodes
        accMax (force -> !curMax) (i, path) = do
            when (i `mod` 100000 == 0) $ putStrLn $ "Iteration " ++ show i
            let release = evalState (runWithPath path (simulatePressureRelease scheduleSingle)) (initState tunnelGraph spLengthMap nodes)
            return $ max curMax release
        allPaths = validPaths spLengthMap (startNode tunnelGraph) nodes
    putStrLn $ allPaths `deepseq` "Found " ++ show (length allPaths) ++ " paths"
    foldM accMax 0 (zip [1..] allPaths)

main :: IO ()
main = do
    (testGraph, nameToNode) <- parseOrDie parseGraph <$> TIO.readFile "aoc22_day16_test"
    let nodes = nonZeroNodes testGraph
        spLengthMap = allPairsShortestPath testGraph nodes
        strToName [c1, c2] = (c1, c2)
        testPath = fmap ((nameToNode Map.!) . strToName) ["BB", "CC"]
        actualTestRelease = evalState (runWithPath testPath (simulatePressureRelease scheduleSingle)) (initState testGraph spLengthMap nodes)
    print actualTestRelease
    let actualMaxRelease = searchMaxPressureRelease testGraph
    print actualMaxRelease
    assert (actualTestRelease == 364 + 52 && actualMaxRelease == 1651) $ do
        (graph, _) <- parseOrDie parseGraph <$> TIO.readFile "aoc22_day16_input"
        let maxRelease = searchMaxPressureRelease graph
        putStrLn $ "Question 1 answer is: " ++ show maxRelease
