{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad.Random.Strict
import Control.Parallel.Strategies
import Data.Array.Repa hiding ((++))
import Data.Array.Repa qualified as R
import Data.Array.Repa.Repr.Unboxed (Unbox)
import Data.Hashable
import Data.IntMap qualified as IMap
import Data.Text.IO qualified as TIO
import Imports
import ParetoFront
import ParseUtils
import Text.Read (readMaybe)

blueprintsParser :: Parsec Void Text (IntMap Blueprint)
blueprintsParser = fmap IMap.fromList $
  some $ do
    void $ string "Blueprint "
    idx <- decimal
    void $ string ": Each ore robot costs "
    oreRobotCost <- decimal
    void $ string " ore. Each clay robot costs "
    clayRobotCost <- decimal
    void $ string " ore. Each obsidian robot costs "
    obsidianRobotOreCost <- decimal
    void $ string " ore and "
    obsidianRobotClayCost <- decimal
    void $ string " clay. Each geode robot costs "
    geodeRobotOreCost <- decimal
    void $ string " ore and "
    geodeRobotObsidianCost <- decimal
    void $ string " obsidian."
    void newline
    return
      ( idx,
        bluePrintFromResMaps
          [ (Ore, resMapFromList [(Ore, oreRobotCost)]),
            (Clay, resMapFromList [(Ore, clayRobotCost)]),
            (Obsidian, resMapFromList [(Ore, obsidianRobotOreCost), (Clay, obsidianRobotClayCost)]),
            (Geode, resMapFromList [(Ore, geodeRobotOreCost), (Obsidian, geodeRobotObsidianCost)])
          ]
      )

resMapFromList :: [(Resource, Int)] -> ResourceMap
resMapFromList assocs =
  let resIMap = IMap.fromList $ fmap (first fromEnum) assocs
   in computeUnboxedS $ fromFunction (Z :. 4) (\(Z :. i) -> fromMaybe 0 (IMap.lookup i resIMap))

bluePrintFromResMaps :: [(Resource, ResourceMap)] -> Blueprint
bluePrintFromResMaps assocs =
  let resIMap = IMap.fromList $ fmap (first fromEnum) assocs
   in computeUnboxedS $ fromFunction (Z :. 4 :. 4) (\(Z :. i :. j) -> (resIMap IMap.! i) R.! (Z :. j))

-- Map a robot type to cost for each resource
-- We map each of the 4 resource to a 0-3 index,
-- and store:

-- * Blueprints as 4x4 matrices mapping a robot

--   resource to its cost vector

-- * Robots as 4d vector mapping of resources to number of robots outputting that resource

-- * Current resources as 4d vector mapping of resources to amount of that resource

type Blueprint = Array U DIM2 Int

type ResourceMap = Array U DIM1 Int

data Resource = Ore | Clay | Obsidian | Geode
  deriving (Generic, Eq, Ord, Show, Bounded, Enum)

instance NFData Resource

instance Hashable Resource

-- Instance from newer source code of Hashable.
liftHashWithSalt h s m =
  IMap.foldlWithKey'
    (\s' k v -> h (hashWithSalt s' k) v)
    (hashWithSalt s (IMap.size m))
    m

hashWithSalt1 = liftHashWithSalt hashWithSalt

instance Hashable a => Hashable (IntMap a) where
  hashWithSalt = hashWithSalt1

instance (Hashable a, Unbox a, Integral a, Shape sh) => Hashable (Array U sh a) where
  hashWithSalt salt arr =
    foldAllS hashWithSalt (hashWithSalt salt $ R.size $ R.extent arr) $ R.map fromIntegral arr

data SimState = SimState
  { -- quantity of each resource
    resources :: !ResourceMap,
    -- quantity of each robot
    robots :: !ResourceMap,
    prevMAction :: !(Maybe Action),
    timeRemaining :: !Int
  }
  deriving (Eq, Generic, Show)

instance Hashable SimState

instance NFData SimState where
  rnf SimState {..} =
    resources `deepSeqArray` robots `deepSeqArray` prevMAction `deepseq` timeRemaining `deepseq` ()

initState :: Int -> SimState
initState timeRemaining =
  SimState
    { resources = resMapFromList $ zip [Ore .. Geode] (repeat 0),
      robots = resMapFromList [(Ore, 1), (Clay, 0), (Obsidian, 0), (Geode, 0)],
      prevMAction = Nothing,
      timeRemaining = timeRemaining
    }

data Action = CreateRobot !Int
  deriving (Generic, Eq, Ord, Show)

instance NFData Action

instance Hashable Action

type Sim = StateT SimState (Reader Blueprint)

runSim :: Blueprint -> SimState -> Sim a -> (a, SimState)
runSim blueprint simState = flip runReader blueprint . flip runStateT simState

legalActions :: Blueprint -> SimState -> [Maybe Action]
legalActions blueprint simState = fst $
  runSim blueprint simState $ do
    curResources <- use #resources
    blueprint <- ask
    let tiledResources = extend (Z :. (4 :: Int) :. All) curResources
        res = foldS (&&) True $ R.zipWith (>=) tiledResources blueprint
    return $ Nothing : fmap (Just . CreateRobot . fst) (filter snd $ zip [fromEnum Ore .. fromEnum Geode] (R.toList res))

oneHot :: IntMap ResourceMap
oneHot =
  IMap.fromList $
    [ (res, computeUnboxedS $ fromFunction (Z :. 4) (\(Z :. i) -> if i == res then 1 else 0))
      | res <- [fromEnum Ore .. fromEnum Geode]
    ]

payCost :: Maybe Action -> Sim ()
payCost mAction = forM_ mAction $ \(CreateRobot res) -> do
  blueprint <- ask
  #resources %= computeUnboxedS . (-^ slice blueprint (Z :. res :. All))

createRobot :: Maybe Action -> Sim ()
createRobot mAction = forM_ mAction $ \(CreateRobot res) -> do
  #robots %= computeUnboxedS . (+^ oneHot IMap.! res)

{-# SCC simulate #-}
simulate :: Maybe Action -> Sim (Maybe Int)
simulate mAction = do
  minutesRemaining <- use #timeRemaining
  if minutesRemaining == 0
    then do
      finalResources <- use #resources
      return $ Just $ finalResources R.! ix1 (fromEnum Geode)
    else do
      prevMAction <- use #prevMAction
      -- Pay the cost now, receive the robot next step.
      payCost mAction
      createRobot prevMAction
      curRobots <- use #robots
      #resources %= computeUnboxedS . (+^ curRobots)
      #timeRemaining -= 1
      #prevMAction .= mAction
      return Nothing

data MemoizeState = MemoizeState
  { cache :: ParetoFront (Maybe Int),
    iteration :: Int
  }
  deriving (Generic)

instance NFData MemoizeState

data SearchHeuristic = Ascending | Descending
  deriving (Generic, Eq, Show, Data, Typeable)

data SearchConfig = SearchConfig
  { heuristic :: SearchHeuristic
  }
  deriving (Generic)

type Search = ReaderT SearchConfig (RandT StdGen (State MemoizeState))

simStateToElem :: SimState -> Element
simStateToElem SimState {..} =
  [timeRemaining]
    ++ ( case prevMAction of
           Nothing -> [0, 0, 0, 0]
           Just (CreateRobot r) -> R.toList (oneHot IMap.! fromIntegral r)
       )
    ++ R.toList robots
    ++ R.toList resources

-- Memoizing function for dynamic programming, which also stores a pareto
-- front of encountered simulation states such that dominated state do not
-- get explored. Memory usage should be kept in check by this as well, as
-- dominated states get removed from the pareto front.
{-# SCC memoize #-}
memoize :: (SimState -> Search (Maybe Int)) -> SimState -> Search (Maybe Int)
memoize f x = do
  isCleanupIt <- (== 0) . (`mod` 100000) <$> use #iteration
  #iteration += 1
  when isCleanupIt $ do
    st <- get
    st `deepseq` do
      -- this ugly traceM call is just there in case this gets optimized
      -- out otherwise by GHC. I'm not sure it's necessary, but I'm not taking
      -- any chances.
      traceM "Cleaning up!"
      return ()
  cache <- use #cache
  let xElem = simStateToElem x
      (lookupRes, newCache, isDominated) = insertLookup xElem cache
  #cache .= newCache
  case lookupRes of
    Just y -> return $ Just y
    Nothing -> do
      -- If the state is dominated by another one in the cache, there is no
      -- need to explore it, so we return Nothing.
      if isDominated
        then return Nothing
        else do
          y <- f x
          -- Once we have computed the function, store its value in the pareto
          -- front.
          #cache %= frontInsert xElem y
          return y

runMemoize :: SearchConfig -> StdGen -> Search a -> a
runMemoize conf gen memAct =
  evalState (evalRandT (runReaderT memAct conf) gen) $
    MemoizeState
      { cache = frontEmpty,
        iteration = 0
      }

{-# SCC maxNumGeodes #-}
maxNumGeodes :: SearchConfig -> StdGen -> Int -> Blueprint -> Int
maxNumGeodes conf gen minutesRemaining blueprint =
  let maxNumGeodes' :: SimState -> Search (Maybe Int)
      maxNumGeodes' (force -> !simState) = do
        let actions = legalActions blueprint simState
        heuristic <- view #heuristic
        heuristicResults <- forM (zip [0 ..] actions) $ \(i, act) -> do
          let (mRes, st') = runSim blueprint simState $ simulate act
          case heuristic of
            Ascending -> return (- (fromIntegral i), st', mRes)
            Descending -> return (fromIntegral i, st', mRes)
        childResults <-
          fmap catMaybes $
            forM (sortBy (\(v1, _, _) (v2, _, _) -> compare v2 v1) heuristicResults) $ \(_, st', mRes) -> do
              case mRes of
                Just v -> return $ Just v
                Nothing -> memoize maxNumGeodes' st'
        if null childResults then return Nothing else return $ Just $ maximum childResults
   in fromJust $ runMemoize conf gen $ maxNumGeodes' (initState minutesRemaining)

sumQualityLevel :: SearchConfig -> StdGen -> Int -> IntMap Blueprint -> Int
sumQualityLevel conf gen minutes blueprints =
  sum $ uncurry (*) <$> (fmap (second (maxNumGeodes conf gen minutes)) (IMap.assocs blueprints) `using` parList rdeepseq)

data Mode = SolveQ1 | Play | Test
  deriving (Generic, Show, Data, Typeable, Eq)

data Args = Args
  { mode :: Mode,
    searchHeuristic :: SearchHeuristic
  }
  deriving (Generic, Show, Data, Typeable)

main :: IO ()
main = do
  args <- cmdArgs $ Args {mode = SolveQ1, searchHeuristic = Descending}
  testBlueprints <- parseOrDie blueprintsParser <$> TIO.readFile "aoc22_day19_test"
  blueprints <- parseOrDie blueprintsParser <$> TIO.readFile "aoc22_day19_input"
  -- RNG only used for random rollout heuristic, so sharing shouldn't be an issue.
  gen <- getStdGen
  let conf =
        SearchConfig
          { heuristic = searchHeuristic args
          }
  case mode args of
    SolveQ1 -> do
      putStrLn $ "Question 1 answer is: " ++ show (sumQualityLevel conf gen 24 blueprints)
    Play -> play (testBlueprints IMap.! 1) (initState 24)
    Test -> do
      -- Some simple unit tests for the pareto front
      let initFront = frontSingleton [1, 2, 3] Nothing :: ParetoFront (Maybe Int)
      assert (length initFront == 1) $ return ()
      let insert1Res = insertLookup [0, 0, 0] initFront
      assert (insert1Res == (Nothing, initFront, True)) $ return ()
      let (actualLookup, actualFront, isDominated) = insertLookup [3, 2, 1] initFront
      assert (frontAssocs actualFront == [([1, 2, 3], Nothing), ([3, 2, 1], Nothing)]) $ return ()
      assert (isNothing actualLookup && not isDominated) $ return ()
      let (_, actualFront', _) = insertLookup [2, 2, 3] actualFront
      assert (frontAssocs actualFront' == [([2, 2, 3], Nothing), ([3, 2, 1], Nothing)]) $ return ()
      let actualFront'' = frontInsert [2, 2, 3] (Just 5) actualFront'
          (actualLookup'', _, _) = insertLookup [2, 2, 3] actualFront''
      assert (actualLookup'' == Just 5) $ return ()
      -- Check results on the test blueprints
      startTime <- getTime
      let actualSumQL = sumQualityLevel conf gen 20 testBlueprints
      endTime <- actualSumQL `deepseq` getTime
      putStrLn $ "Ran in " ++ show (endTime - startTime) ++ " seconds"
      putStrLn $ "Test sum QL: " ++ show actualSumQL
      assert (actualSumQL == 33) $ return ()

showAction :: Maybe Action -> String
showAction Nothing = "do nothing"
showAction (Just (CreateRobot res)) =
  "create " ++ show (toEnum (fromIntegral res) :: Resource) ++ " robot"

showResMap :: ResourceMap -> String
showResMap resmap =
  foldr (\(res, j) curStr -> show res ++ ": " ++ show j ++ ", " ++ curStr) "" $ zip [Ore .. Geode] $ R.toList resmap

play :: Blueprint -> SimState -> IO ()
play blueprint state = do
  let actions = legalActions blueprint state
      actionMap = zip [0 ..] actions
  putStrLn $ "Blueprint: "
  forM_ [Ore .. Geode] $ \res -> do
    let costMap = computeUnboxedS $ slice blueprint (Z :. fromEnum res :. All)
    putStrLn $ "\t" ++ show res ++ " robot: " ++ showResMap costMap
  putStrLn $ "Resources: " ++ showResMap (resources state)
  putStrLn $ "Robots: " ++ showResMap (robots state)
  putStrLn $ "Time remaining: " ++ show (timeRemaining state) ++ " minutes"
  putStrLn $ "Actions: " ++ foldr (\(i, act) curStr -> show i ++ ": " ++ showAction act ++ ", " ++ curStr) "" actionMap
  mIdx <- readMaybe <$> getLine
  case mIdx >>= \idx -> lookup idx actionMap of
    Nothing -> play blueprint state
    Just mAction -> do
      let (mRes, state') = runSim blueprint state (simulate mAction)
      case mRes of
        Just res -> putStrLn $ "Game finished with outcome: " ++ show res
        Nothing -> do
          play blueprint state'
