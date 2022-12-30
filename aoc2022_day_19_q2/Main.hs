module Main (main) where

import Data.Foldable
import Data.IntMap.Strict qualified as IMap
import Data.Text.IO qualified as TIO
import Imports as I
import ParseUtils
import Z3.Monad as Z3

type ResourceMap = (AST, AST, AST, AST)

type Blueprint = (ResourceMap, ResourceMap, ResourceMap, ResourceMap)

ore = _1

clay = _2

obsidian = _3

geode = _4

zipMaps :: (AST -> AST -> Z3 AST) -> ResourceMap -> ResourceMap -> Z3 ResourceMap
zipMaps fn (or1, c1, ob1, g1) (or2, c2, ob2, g2) =
  (,,,) <$> fn or1 or2 <*> fn c1 c2 <*> fn ob1 ob2 <*> fn g1 g2

addMaps :: ResourceMap -> ResourceMap -> Z3 ResourceMap
addMaps = let add a b = mkAdd [a, b] in zipMaps add

subMaps :: ResourceMap -> ResourceMap -> Z3 ResourceMap
subMaps = let sub a b = mkSub [a, b] in zipMaps sub

data SimState = SimState
  { resources :: ResourceMap,
    robots :: ResourceMap,
    -- Actions have a cost and some resources we add
    -- to the robots
    prevAction :: ResourceMap
  }
  deriving (Generic)

initState :: Z3 SimState
initState = do
  zero <- mkInteger 0
  let zeroMap = (zero, zero, zero, zero)
  one <- mkInteger 1
  return $
    SimState
      { resources = zeroMap,
        robots = zeroMap & ore .~ one,
        prevAction = zeroMap
      }

mkIteMap :: AST -> ResourceMap -> ResourceMap -> Z3 ResourceMap
mkIteMap condition = zipMaps (mkIte condition)

mkCases :: AST -> (ResourceMap, ResourceMap) -> [(AST, (ResourceMap, ResourceMap))] -> Z3 (ResourceMap, ResourceMap)
mkCases x =
  foldrM
    ( \(x', (thenCost, thenGain)) (elseCost, elseGain) -> do
        condition <- mkEq x x'
        (,)
          <$> mkIteMap condition thenCost elseCost
          <*> mkIteMap condition thenGain elseGain
    )

canBePlayed :: ResourceMap -> ResourceMap -> Z3 AST
canBePlayed map1 map2 = do
  (le1, le2, le3, le4) <- zipMaps mkLe map1 map2
  mkAnd [le1, le2, le3, le4]

-- Z3 program to simulate the game using a fixed number of
-- steps, outputting the number of geodes after those minutes.
simulate :: Blueprint -> Int -> SimState -> Z3 AST
simulate _ 0 SimState {..} = do
  return $ resources ^. geode
simulate blueprint timeRemaining simState@(SimState {..}) = do
  -- Choose an action among a maximum of
  -- 5 possibilities: do nothing, or create a robot
  -- of any of the 4 resources.
  zero <- mkInteger 0
  one <- mkInteger 1
  -- Each action is a (cost, robot gain) pair of resource maps
  let zeroMap = (zero, zero, zero, zero)
      doNothing = (zeroMap, zeroMap)
      makeOreRobot =
        ( blueprint ^. ore,
          zeroMap & ore .~ one
        )
      makeClayRobot =
        ( blueprint ^. clay,
          zeroMap & clay .~ one
        )
      makeObsRobot =
        ( blueprint ^. obsidian,
          zeroMap & obsidian .~ one
        )
      makeGeodeRobot =
        ( blueprint ^. geode,
          zeroMap & geode .~ one
        )
  -- We let Z3 pick the index of the action
  actionIdx <- mkFreshIntVar ("action at " ++ show timeRemaining)
  mkLe zero actionIdx >>= Z3.assert
  two <- mkInteger 2
  three <- mkInteger 3
  four <- mkInteger 4
  mkLe actionIdx four >>= Z3.assert
  (actionCost, actionGain) <-
    mkCases
      actionIdx
      makeGeodeRobot
      [ (zero, doNothing),
        (one, makeOreRobot),
        (two, makeClayRobot),
        (three, makeObsRobot)
      ]
  -- Assert that the picked action can acually be played.
  canBePlayed actionCost resources >>= Z3.assert
  -- Subtract the cost from the resource now.
  newResources <- subMaps resources actionCost
  -- Create robots from the previous action if any.
  newRobots <- addMaps robots prevAction
  -- Create resources with the current robots.
  newResources' <- addMaps newResources newRobots
  let newState =
        simState
          & #resources .~ newResources'
          & #robots .~ newRobots
          & #prevAction .~ actionGain
  simulate blueprint (timeRemaining - 1) newState

type ResourceMapSpec = (Integer, Integer, Integer, Integer)

type BlueprintSpec = (ResourceMapSpec, ResourceMapSpec, ResourceMapSpec, ResourceMapSpec)

blueprintsParser :: Parsec Void Text (IntMap BlueprintSpec)
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
    let zeroMap = (0, 0, 0, 0)
        zeroBlueprint = (zeroMap, zeroMap, zeroMap, zeroMap)
    return
      ( idx,
        zeroBlueprint
          & ore . ore .~ oreRobotCost
          & clay . ore .~ clayRobotCost
          & obsidian . ore .~ obsidianRobotOreCost
          & obsidian . clay .~ obsidianRobotClayCost
          & geode . ore .~ geodeRobotOreCost
          & geode . obsidian .~ geodeRobotObsidianCost
      )

mkResourceMap :: ResourceMapSpec -> Z3 ResourceMap
mkResourceMap (ore, clay, obsidian, geode) = do
  (,,,)
    <$> mkInteger ore
    <*> mkInteger clay
    <*> mkInteger obsidian
    <*> mkInteger geode

mkBlueprint :: BlueprintSpec -> Z3 Blueprint
mkBlueprint (ore, clay, obsidian, geode) = do
  (,,,)
    <$> mkResourceMap ore
    <*> mkResourceMap clay
    <*> mkResourceMap obsidian
    <*> mkResourceMap geode

checkFeasible :: BlueprintSpec -> Int -> Integer -> IO Bool
checkFeasible blueprintSpec timeRemaining targetNumGeodes = do
  evalZ3 $ do
    blueprint <- mkBlueprint blueprintSpec
    startState <- initState
    numGeodes <- simulate blueprint timeRemaining startState
    target <- mkInteger targetNumGeodes
    mkEq numGeodes target >>= Z3.assert
    fmap (isJust . join . snd) $ withModel $ \m -> evalInt m numGeodes

findBlueprintMax :: BlueprintSpec -> Int -> IO Integer
findBlueprintMax blueprintSpec timeRemaining = do
  -- Rough upper bound: what if we create a geode robot every
  -- step
  let upperBound = sum [1 .. fromIntegral timeRemaining -1]
  -- We bnary search for the maximum value within 0 and upperBound.
  let search minT maxT = do
        let target = ((minT + maxT) `div` 2) + 1
        if minT == maxT
          then return minT
          else do
            isFeasible <- checkFeasible blueprintSpec timeRemaining target
            if isFeasible
              then do
                search target maxT
              else do
                search minT (target - 1)
  search 0 upperBound

main :: IO ()
main = do
  testBlueprints <- parseOrDie blueprintsParser <$> TIO.readFile "aoc22_day19_test"
  actualMax1 <- findBlueprintMax (testBlueprints ^?! ix 1) 32
  I.assert (actualMax1 == 56) $ return ()
  actualMax2 <- findBlueprintMax (testBlueprints ^?! ix 2) 32
  I.assert (actualMax2 == 62) $ return ()
  blueprints <- parseOrDie blueprintsParser <$> TIO.readFile "aoc22_day19_input"
  actualMaxes <- forM [1 .. 3] $ \i -> findBlueprintMax (blueprints ^?! ix i) 32
  putStrLn $ "Question 2 answer is: " ++ show (product actualMaxes)
