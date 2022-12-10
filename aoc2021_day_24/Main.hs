module Main where

import Data.Array.Accelerate (Acc, Arrays, DIM1, Elt, Exp (..), Scalar, Vector, Z (..), constant, (:.) (..))
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Control.Lens.Tuple ()
import Data.Array.Accelerate.LLVM.PTX (run, runN)
import Data.Text.IO qualified as TIO
import Imports

-- An ALU program takes as input:

-- * Input number: a (14,) shaped array representing the input number

-- * ALU state: a (4,) shaped array representing the ALU state:

--   - 0-3: registers w, x, y, z
-- And outputs the updated ALU state.
-- Using tuples since that's what Accelerate supports as scalar types.
type ALUState = (Int, Int, Int, Int)

type ALUInput = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

type ALUProgram = Exp ALUInput -> Exp ALUState -> Exp ALUState

type Var = Int

w, x, y, z :: Var
w = 0
x = 1
y = 2
z = 3

-- A whole lot of type machinery so we can use convenient ix lens notation to work with tuple
-- with Accelerate (statically as far as Accelerate is concerned)
instance
  ( a ~ a2,
    a ~ a3,
    a ~ a4,
    a ~ a5,
    a ~ a6,
    a ~ a7,
    a ~ a8,
    a ~ a9,
    a ~ a10,
    a ~ a11,
    a ~ a12,
    a ~ a13,
    a ~ a14,
    b ~ b2,
    b ~ b3,
    b ~ b4,
    b ~ b5,
    b ~ b6,
    b ~ b7,
    b ~ b8,
    b ~ b9,
    b ~ b10,
    b ~ b11,
    b ~ b12,
    b ~ b13,
    b ~ b14
  ) =>
  Each (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) (b, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14) a b
  where
  each p ~(a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (,,,,,,,,,,,,,) <$> p a <*> p b <*> p c <*> p d <*> p e <*> p f <*> p g <*> p h <*> p i <*> p j <*> p k <*> p l <*> p m <*> p n

type instance Index (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = Int

type instance IxValue (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = a

instance (a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10, a ~ a11, a ~ a12, a ~ a13, a ~ a14) => Ixed (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
  ix p = elementOf each p

type instance Index (Exp (a, a2, a3, a4)) = Int

type instance IxValue (Exp (a, a2, a3, a4)) = Exp a

instance (Elt a, a ~ a2, a ~ a3, a ~ a4) => Ixed (Exp (a, a2, a3, a4)) where
  ix p = liftLens (elementOf each p :: Traversal' (Exp (a, a2, a3, a4)) (IxValue (Exp (a, a2, a3, a4))))

instance (Elt a, Elt b) => Each (Exp (a, a, a, a, a, a, a, a, a, a, a, a, a, a)) (Exp (b, b, b, b, b, b, b, b, b, b, b, b, b, b)) (Exp a) (Exp b) where
  each = liftLens (each :: Traversal (Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a, Exp a) (Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b, Exp b) (Exp a) (Exp b))

type instance Index (Exp (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)) = Int

type instance IxValue (Exp (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)) = Exp a

instance (Elt a, a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9, a ~ a10, a ~ a11, a ~ a12, a ~ a13, a ~ a14) => Ixed (Exp (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)) where
  ix p = liftLens (elementOf each p :: Traversal' (Exp (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)) (IxValue (Exp (a, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))))

-- Basic instruction definitions
inp :: Var -> Int -> ALUProgram
inp inpVar inpLoc inputDigits curState =
  curState
    & ix inpVar .~ inputDigits ^?! ix inpLoc

type Operator = Var -> Either Var Int -> ALUProgram

operator :: (Exp Int -> Exp Int -> Exp Int) -> Operator
operator updateOp dst src _ curState = case src of
  Left srcVar -> curState & ix dst %~ \i -> updateOp i $ curState ^?! ix srcVar
  Right srcVal -> curState & ix dst %~ \i -> updateOp i (constant srcVal)

-- Parsing ALU programs
newtype ParserState = ParserState
  { curInpIndex :: Int
  }
  deriving (Generic)

type Parser = ParsecT Void Text (State ParserState)

parseVar :: Parser Var
parseVar = do
  varChar <- oneOf ("wxyz" :: String)
  return $ case varChar of
    'w' -> w
    'x' -> x
    'y' -> y
    'z' -> z
    _ -> error "this should never happen!"

parseOp :: Text -> Operator -> Parser ALUProgram
parseOp name op = do
  string name
  hspace
  dst <- parseVar
  hspace
  src <- fmap Left parseVar <|> fmap Right (signed hspace decimal)
  return $ op dst src

parseInp :: Parser ALUProgram
parseInp = do
  string "inp"
  hspace
  inpVar <- parseVar
  inpIndex <- use #curInpIndex
  #curInpIndex += 1
  return $ inp inpVar inpIndex

sequenceALU :: ALUProgram -> ALUProgram -> ALUProgram
sequenceALU p1 p2 inputDigits curState =
  p2 inputDigits (p1 inputDigits curState)

parseALUProgram :: Parser ALUProgram
parseALUProgram = fmap (foldr1 sequenceALU) $
  many $ do
    op <-
      foldr1 (<|>) $
        parseInp :
          [ parseOp name op
            | (name, op) <-
                [ ("add", operator (+)),
                  ("mul", operator (*)),
                  ("div", operator div),
                  ("mod", operator mod),
                  ("eql", operator (\i j -> 1 - min 1 (abs (i - j)))) -- Accelerate doesn't define Enum so we have to hack out eql...
                ]
          ]
    newline
    return op

parseALUprogramOrDie :: Text -> ALUProgram
parseALUprogramOrDie input =
  case evalState (runParserT parseALUProgram "" input) $ ParserState 0 of
    Left err -> error $ errorBundlePretty err
    Right program -> program

-- Accelerate program to execute an ALU program with many inputs in parallel on GPU
listToALUState :: [Exp Int] -> Exp ALUInput
listToALUState list =
  case list of
    [a, b, c, d, e, f, g, h, i, j, k, l, m, n] -> A.lift (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    _ -> error "Invalid list length, need exactly 14 digits!"

-- We generate the input array on GPU directly for efficiency in parallel.
-- While generating them in pure Haskell is much simpler and more readable, it becomes
-- a performance bottleneck.
generateALUInput :: Bool -> Int -> Acc (Scalar Int) -> Acc (Vector ALUInput)
generateALUInput findLargest batchSize startNumber =
  A.generate (constant $ Z :. batchSize) (generateALUInput' findLargest startNumber)

-- We expect simple integers as input, and convert them to nonary representation,
-- adding to ensure that they are in the 1 to 9 range.
accDigits :: Int -> (Exp Int, [Exp Int]) -> (Exp Int, [Exp Int])
accDigits expVal (curRemainder, curDigits) =
  let denom = constant (9 ^ expVal)
   in (curRemainder `rem` denom, (curRemainder `div` denom) + 1 : curDigits)

generateALUInput' :: Bool -> Acc (Scalar Int) -> Exp DIM1 -> Exp ALUInput
generateALUInput' findLargest startingNumber idx =
  let i = A.unindex1 idx
      outNumber = A.the startingNumber + if findLargest then - i else i
      aluInput = listToALUState $ reverse $ snd $ foldr accDigits (outNumber, []) [0 .. 13]
   in aluInput

runALUProgram :: ALUProgram -> Int -> Acc (Scalar Int) -> Acc (Vector ALUState)
runALUProgram program batchSize startNumber =
  let aluInputs = generateALUInput True batchSize startNumber
   in runWithALUInputs program batchSize aluInputs

runWithALUInputs :: ALUProgram -> Int -> Acc (Vector ALUInput) -> Acc (Vector ALUState)
runWithALUInputs program batchSize aluInputs =
  let initStates :: Vector ALUState
      initStates = A.fromList (Z :. batchSize) $ repeat (0, 0, 0, 0)
      finalStates = A.zipWith program aluInputs (A.use initStates)
   in finalStates

-- Finding outputs with z == 0 is also implemented in parallel on GPU, as a pure Haskell
-- solution was much too slow.
runALUProgramAndFindZeros' :: Bool -> ALUProgram -> Int -> Acc (Scalar Int) -> Acc (Vector ALUInput, Scalar Int)
runALUProgramAndFindZeros' findLargest program batchSize startNumber =
  let aluInputs = generateALUInput findLargest batchSize startNumber
      finalStates = runWithALUInputs program batchSize aluInputs
      zeroFinalStates = A.map (\aluState -> aluState ^?! ix z A.== constant 0) finalStates
      zeroInputs = A.compact zeroFinalStates aluInputs
   in zeroInputs

parseNonary :: String -> Int
parseNonary = fst . head . readInt 9 (`elem` ['1' .. '9']) (\c -> digitToInt c - 1)

findZeroNumber :: Bool -> Int -> ALUProgram -> IO (Maybe (Vector ALUInput))
findZeroNumber findLargest batchSize aluProgram = do
  let aluFunctionR = runN $ runALUProgramAndFindZeros' findLargest aluProgram batchSize
      largestNumber = parseNonary "99999999999999"
      startNumbers =
        if findLargest
          then [largestNumber, largestNumber - batchSize .. 0]
          else [0, 0 + batchSize .. largestNumber]
      totalNumIter = length startNumbers
      doFind mPrevTime mMovingAvg inputStartNumbers = do
        case inputStartNumbers of
          ((i, curStartNumber) : rest) -> do
            let batchOutputs = fst $ aluFunctionR (A.fromList Z [curStartNumber])
            endTime <- batchOutputs `deepseq` getTime
            newMovingAvg <- case (mPrevTime, mMovingAvg) of
              (Nothing, _) -> return Nothing
              (Just prevTime, Nothing) -> return $ Just (endTime - prevTime)
              (Just prevTime, Just movingAvg) -> do
                let updatedItTime = 0.01 * (endTime - prevTime) + 0.99 * movingAvg
                when (i `mod` 1000 == 0) $ do
                  putStrLn $ "Iteration " ++ show i ++ " out of " ++ show totalNumIter
                  putStrLn $ "ETA: " ++ show (fromIntegral (totalNumIter - i) * updatedItTime) ++ " seconds"
                return $ Just updatedItTime
            if A.arraySize batchOutputs > 0
              then return $ Just batchOutputs
              else doFind (Just endTime) newMovingAvg rest
          [] -> return Nothing
  doFind Nothing Nothing $ zip [0 ..] startNumbers

-- Because the search is very time consuming in the case of question 1,
-- (about 2 hours using a Nvidia 2080 Ti) the questions are answered with a command
-- flag specific to the search direction.
data SearchMode = Max | Min
  deriving (Generic, Show, Data, Typeable, Eq)

-- Batch size defaults to 10M. Should be adjusted depending on GPU vram.
-- Higher is better.
data Args = Args
  { mode :: SearchMode,
    batchSize :: Int
  }
  deriving (Generic, Show, Data, Typeable)

-- Running a bunch of tests before running the real thing.
main :: IO ()
main = do
  args <- cmdArgs $ Args {mode = Min, batchSize = 10000000}
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  testAluProgram <- parseALUprogramOrDie <$> TIO.readFile "aoc21_day24_test"
  let testInput1 = A.use $ A.fromList Z [parseNonary "99999999994999"]
      actualGenerateOut = run (generateALUInput True 1 testInput1)
  assert (A.toList actualGenerateOut == [(9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 4, 9, 9, 9)]) $ do
    let actualFinalState1 = run (runALUProgram testAluProgram 1 testInput1)
    assert (A.toList actualFinalState1 == [(9, 0, 0, 0)]) $ do
      let actualFinalState2 = run (runALUProgram testAluProgram 1 $ A.use $ A.fromList Z [parseNonary "99999999996999"])
      assert (A.toList actualFinalState2 == [(9, 0, 0, 2)]) $ do
        actualLargestZ1Number <- findZeroNumber True (args ^. #batchSize) testAluProgram
        assert ((head . A.toList <$> actualLargestZ1Number) == Just (9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 9, 9, 9)) $ do
          testAluProgram2 <- parseALUprogramOrDie <$> TIO.readFile "aoc21_day24_test4"
          let actualFinalState3 = run (runALUProgram testAluProgram2 1 $ A.use $ A.fromList Z [parseNonary "26111111111111"])
          assert (A.toList actualFinalState3 == [(0, 6, 0, 1)]) $ do
            let actualFinalState4 = run (runALUProgram testAluProgram2 1 $ A.use $ A.fromList Z [parseNonary "27111111111111"])
            assert (A.toList actualFinalState4 == [(0, 7, 0, 0)]) $ do
              aluProgram <- parseALUprogramOrDie <$> TIO.readFile "aoc21_day24"
              testAluProgram3 <- parseALUprogramOrDie <$> TIO.readFile "aoc21_day24_test2"
              let actualFinalState5 = run (runALUProgram testAluProgram3 1 $ A.use $ A.fromList Z [parseNonary "49111111111111"])
              assert (head (A.toList actualFinalState5) ^?! ix z == 485) $ do
                let findLargest = args ^. #mode == Max
                largestNumber <- findZeroNumber findLargest (args ^. #batchSize) aluProgram
                putStrLn $ show (args ^. #mode) ++ " accepted value is: " ++ show (head . A.toList <$> largestNumber)
