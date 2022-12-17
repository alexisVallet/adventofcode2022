module Main where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.Nested
import Control.Monad.Coroutine.SuspensionFunctors hiding (Reader)
import Control.Parallel.Strategies
import Data.Functor.Sum
import Data.Graph.Inductive hiding ((&))
import Data.Graph.Inductive qualified as G
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

data NodeLabel = NodeLabel
  { flowRate :: Int,
    name :: (Char, Char)
  }
  deriving (Generic, Show)

data TunnelGraph = TunnelGraph
  { graph :: Gr NodeLabel Int,
    startNode :: Node
  }
  deriving (Generic, Show)

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
    return (NodeLabel {flowRate = flowRate, name = name}, neighbors)
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

data SimConfig = SimConfig
  { totalTime :: Int,
    tunnel :: TunnelGraph,
    spLengthMap :: Map (Node, Node) Int
  }
  deriving (Generic, Show)

data SimState f = SimState
  { playerState :: f PlayerState,
    unexplored :: Set Node
  }
  deriving (Generic)

instance (NFData (f PlayerState)) => NFData (SimState f)

data PlayerState = PlayerState
  { curNode :: Int,
    curFlowRate :: Int
  }
  deriving (Generic, Show)

instance NFData PlayerState

initState :: (Applicative f) => Int -> TunnelGraph -> Map (Node, Node) Int -> [Node] -> (SimConfig, SimState f)
initState totalTime tunnel spLengthMap nonZeroNodes =
  let state =
        SimState
          { playerState =
              pure $
                PlayerState
                  { curNode = startNode tunnel,
                    curFlowRate = 0
                  },
            unexplored = Set.fromList nonZeroNodes
          }
      conf = SimConfig {tunnel = tunnel, spLengthMap = spLengthMap, totalTime = totalTime}
   in (conf, state)

instance (Functor s', MonadState s m) => MonadState s (Coroutine s' m) where
  state f = lift (state f)

instance (Functor s', MonadReader r m) => MonadReader r (Coroutine s' m) where
  ask = lift ask
  reader f = lift $ reader f

-- Coroutine requesting the next node to move to, or Nothing to stay at the
-- current node and bring an early end to the simulation.
type SimMonad f =
  Coroutine
    ( Request
        (Set Node) -- set of nodes to pick the next node to explore
        (Maybe Node) -- chosen node, Nothing to stop here until the end of time
    )
    (StateT (SimState f) (Reader SimConfig))

type PlayerMonad =
  Coroutine
    ( Sum
        (Request (Set Node) (Maybe Node)) -- Same as SimMonad
        ( Request
            (Node, Int) -- Next node the player intends to explore and current time remaining
            (Set Node) -- Set of unexplored nodes after the other player caught up
        )
    )
    (StateT PlayerState (Reader SimConfig))

singlePressureRelease :: PlayerMonad Int
singlePressureRelease = do
  totalTime <- view #totalTime
  doSimulate totalTime
  where
    doSimulate 0 = return 0
    doSimulate minutesRemaining = do
      curNode <- use #curNode
      totalTime <- view #totalTime
      unexplored <- mapSuspension InR $ request (curNode, totalTime - minutesRemaining)
      valveFlowRate <- flowRate . fromJust . flip lab curNode <$> view (#tunnel . #graph)
      curFlowRate <- use #curFlowRate
      let (nextFlowRate, minutesThere) =
            if valveFlowRate == 0
              then (curFlowRate, 0)
              else (curFlowRate + valveFlowRate, 1)
      mNextNode <- mapSuspension InL $ request unexplored
      let terminate = return $ curFlowRate + nextFlowRate * (minutesRemaining - 1)
      case mNextNode of
        Nothing -> terminate
        Just nextNode -> do
          -- traceM $ "curNode is " ++ show curNode ++ ", requested one of " ++ show unexplored ++ ", got " ++ show nextNode
          spLengthMap <- view #spLengthMap
          let minutesMoving = spLengthMap Map.! (curNode, nextNode)
              canMove = minutesRemaining - minutesThere - minutesMoving >= 0
              actuallyMovingMinutes = if canMove then minutesMoving else 0
              newMinutesRemaining = minutesRemaining - minutesThere - actuallyMovingMinutes
          if not canMove
            then terminate
            else do
              #curNode .= nextNode
              #curFlowRate .= nextFlowRate
              rest <- doSimulate newMinutesRemaining
              return $ minutesThere * curFlowRate + actuallyMovingMinutes * nextFlowRate + rest

simulatePressureRelease :: (Applicative f) => (PlayerMonad Int -> SimMonad f (f b)) -> SimMonad f (f b)
simulatePressureRelease scheduler = scheduler singlePressureRelease

runWithPath :: [Node] -> SimMonad f a -> StateT (SimState f) (Reader SimConfig) a
runWithPath path sim = do
  stepOut <- resume sim
  case stepOut of
    Right result -> return result
    Left (Request _ nextStep) -> do
      case path of
        [] -> runWithPath [] (nextStep Nothing)
        (n : ns) -> runWithPath ns (nextStep $ Just n)

stepSim :: SimConfig -> SimState f -> SimMonad f a -> (Either (Request (Set Node) (Maybe Node) (SimMonad f a)) a, SimState f)
stepSim conf curState sim = flip runReader conf $ flip runStateT curState $ resume sim

searchMaxAllPaths :: (NFData (SimState f), Foldable f) => SimConfig -> SimState f -> SimMonad f (f Int) -> Int
searchMaxAllPaths = searchMaxAllPaths' True

searchMaxAllPaths' :: (NFData (SimState f), Foldable f) => Bool -> SimConfig -> SimState f -> SimMonad f (f Int) -> Int
searchMaxAllPaths' parallelize conf curState sim =
  let (stepOut, nextState) = stepSim conf curState sim
   in case stepOut of
        Left (Request possibleNodes nextStep) ->
          let moves = Set.insert Nothing $ Set.map Just possibleNodes
              evalStrat = if parallelize then parList else evalList
           in maximum ((searchMaxAllPaths' False conf nextState . nextStep <$> Set.toList moves) `using` evalStrat rdeepseq)
        Right result -> sum result

searchMaxPressureRelease :: (Foldable f, Applicative f, NFData (SimState f), NFData (f Int)) => Int -> (PlayerMonad Int -> SimMonad f (f Int)) -> TunnelGraph -> IO Int
searchMaxPressureRelease totalTime scheduler tunnel = do
  let nodes = nonZeroNodes tunnel
      spLengthMap = allPairsShortestPath tunnel nodes
      (conf, state) = initState totalTime tunnel spLengthMap nodes
  return $ searchMaxAllPaths conf state (simulatePressureRelease scheduler)

scheduleSingle :: PlayerMonad a -> SimMonad Identity (Identity a)
scheduleSingle coroa =
  fmap Identity $ pogoStickNested handleRightSingle $ mapMonad (zoom $ #playerState . #runIdentity) coroa

handleRightSingle (Request (curNode, _) nextStep) = do
  -- Single player simply updates the set of unexplored nodes
  -- and return it, since no synchronization is necessary.
  unexplored <- #unexplored <%= Set.delete curNode
  nextStep unexplored

data Pair a = Pair
  { l :: a,
    r :: a
  }
  deriving (Generic, Show)

instance NFData a => NFData (Pair a)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  Pair f1 f2 <*> Pair a1 a2 = Pair (f1 a1) (f2 a2)

instance Foldable Pair where
  foldr f b (Pair a1 a2) = foldr f b [a1, a2]

zoomElem :: (Monad m) => Lens' (Pair PlayerState) PlayerState -> StateT PlayerState m a -> StateT (SimState Pair) m a
zoomElem elemLens = zoom $ #playerState . elemLens

stepElem :: Lens' (Pair PlayerState) PlayerState -> PlayerMonad a -> SimMonad Pair (Either (Request (Node, Int) (Set Node) (PlayerMonad a)) a)
stepElem elemLens playerCoro = do
  playerState <- use $ #playerState . elemLens
  let unwrapState = runStateT (resume playerCoro) playerState
  (rawRes, newPlayerState) <- lift $ lift unwrapState
  #playerState . elemLens .= newPlayerState
  case rawRes of
    -- pass results through
    Right res -> return $ Right res
    -- when requesting next node to explore, suspend transparently
    Left (InL (Request unexplored nextStep)) -> do
      mNode <- request unexplored
      stepElem elemLens $ nextStep mNode
    -- when requesting the unexplored nodes, need to synchronize, so outputting the raw results
    Left (InR unexploredReq) -> return $ Left unexploredReq

unsafeStepElem :: Lens' (Pair PlayerState) PlayerState -> PlayerMonad a -> SimMonad Pair (Request (Node, Int) (Set Node) (PlayerMonad a))
unsafeStepElem elemLens coroa = do
  lCoroRes <- stepElem elemLens coroa
  case lCoroRes of
    Right _ -> error "coro ended on initialization, this should never happen!"
    Left req -> return req

schedulePair :: PlayerMonad a -> SimMonad Pair (Pair a)
schedulePair coroa = do
  -- Need to step through the initial states in turn until unexplored nodes are needed.
  Request (lCurNode, lt) lNextStep <- unsafeStepElem #l coroa
  #unexplored %= Set.delete lCurNode
  Request (rCurNode, rt) rNextStep <- unsafeStepElem #r coroa
  #unexplored %= Set.delete rCurNode
  if lt >= rt
    then alternatePair rNextStep lNextStep #r #l lt
    else alternatePair lNextStep rNextStep #l #r rt

alternatePair :: (Set Node -> PlayerMonad a) -> (Set Node -> PlayerMonad a) -> (forall b. Lens' (Pair b) b) -> (forall b. Lens' (Pair b) b) -> Int -> SimMonad Pair (Pair a)
alternatePair thisCoro otherCoro this other targetTime = do
  unexplored <- use #unexplored
  thisStepRes <- stepElem this (thisCoro unexplored)
  case thisStepRes of
    -- This player finished, so we just run the remaining player as if he was alone from there.
    Right thisRes -> do
      unexplored <- use #unexplored
      otherRes <- pogoStickNested handleRightSingle $ mapMonad (zoom $ #playerState . other) (otherCoro unexplored)
      return $ pure thisRes & other .~ otherRes
    -- This player performed one step, returning current node and current remaining time.
    -- We update the unexplored nodes by removing the current node.
    -- If we went past the target time, we can safely run the other player's coro.
    -- Otherwise, we keep stepping through this player until we do reach the target time.
    Left (Request (curNode, curTime) nextStep) -> do
      #unexplored %= Set.delete curNode
      if curTime >= targetTime
        then do
          alternatePair otherCoro nextStep other this curTime
        else do
          alternatePair nextStep otherCoro this other targetTime

main :: IO ()
main = do
  (testGraph, nameToNode) <- parseOrDie parseGraph <$> TIO.readFile "aoc22_day16_test"
  let nodes = nonZeroNodes testGraph
      spLengthMap = allPairsShortestPath testGraph nodes
      strToName [c1, c2] = (c1, c2)
      testPath = fmap ((nameToNode Map.!) . strToName) ["BB", "CC"]
      (conf, state) = initState 30 testGraph spLengthMap nodes
      actualTestRelease = flip runReader conf $ evalStateT (runWithPath testPath (simulatePressureRelease scheduleSingle)) state
  print actualTestRelease
  actualMaxReleaseSingle <- searchMaxPressureRelease 30 scheduleSingle testGraph
  actualMaxReleasePair <- searchMaxPressureRelease 26 schedulePair testGraph
  print actualMaxReleasePair
  assert
    ( actualTestRelease == 364 + 52
        && actualMaxReleaseSingle == 1651
        && actualMaxReleasePair == 1707
    )
    $ do
      (graph, _) <- parseOrDie parseGraph <$> TIO.readFile "aoc22_day16_input"
      maxReleaseSingle <- searchMaxPressureRelease 30 scheduleSingle graph
      putStrLn $ "Question 1 answer is: " ++ show maxReleaseSingle
      maxReleasePair <- searchMaxPressureRelease 26 schedulePair graph
      putStrLn $ "Question 2 answer is: " ++ show maxReleasePair
