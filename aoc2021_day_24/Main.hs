module Main where

import Data.Maybe
import Debug.Trace
import Data.List
import Data.List.Split hiding (oneOf)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Void
import Data.Vector.Unboxed (Vector, (!))
import Data.Vector.Unboxed qualified as V
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Array.Repa (Array, U, D, DIM1, ix2, ix1, Z(..), (:.)(..))
import Data.Array.Repa qualified as R
import Data.Array.Repa.Eval qualified as R
import Data.Array.Repa.Repr.Vector
import Criterion.Measurement
import Control.DeepSeq

import ParseUtils

-- An ALU program takes as input:
-- * Input number: a (batchSize, 14) shaped array representing the input number
-- * ALU state: a (batchSize, 5) shaped array representing the ALU state:
--   - 0-3: registers w, x, y, z
--   - 4: index position of the next number to read in the input
-- And outputs the updated ALU state.
type ALUState = (Int, Int, Int, Int, Int)
type ALUProgram = ReaderT (Vector Int) (State ALUState) ()
type Var = Int

w, x, y, z, curInp :: Var
w = 0
x = 1
y = 2
z = 3
curInp = 4

inp :: Var -> ALUProgram
inp inpVar = do
    inputDigits <- ask
    modify $ \curState -> curState 
        & ix inpVar .~ inputDigits ^?! ix (curState ^?! ix curInp)
        & ix curInp +~ 1

type Operator = Var -> Either Var Int -> ALUProgram

operator :: (Int -> Int -> Int) -> Operator
operator updateOp dst src = case src of 
    Left srcVar -> do
        modify $ \curState -> curState & ix dst %~ \i -> updateOp i $ curState ^?! ix srcVar
    Right srcVal -> do
        modify $ \curState -> curState & ix dst %~ \i -> updateOp i srcVal

parseVar :: Parsec Void Text Var
parseVar = do
    varChar <- oneOf ("wxyz" :: String)
    return $ case varChar of
        'w' -> w
        'x' -> x
        'y' -> y
        'z' -> z
        _ -> error "this should never happen!"

parseOp :: Text -> Operator -> Parsec Void Text ALUProgram
parseOp name op = do
    string name
    hspace
    dst <- parseVar
    hspace
    src <- fmap Left parseVar <|> fmap Right (signed hspace decimal)
    return $ op dst src

parseInp :: Parsec Void Text ALUProgram
parseInp = do
    string "inp"
    hspace
    inp <$> parseVar

parseALUProgram :: Parsec Void Text ALUProgram
parseALUProgram = fmap (foldr1 (>>)) $ many $ do 
    op <- foldr1 (<|>) $ parseInp : [parseOp name op | (name, op) <- [
        ("add", operator (+)),
        ("mul", operator (*)),
        ("div", operator div),
        ("mod", operator mod),
        ("eql", operator (\i j -> fromEnum $ i == j))]]
    newline
    return op

runALUProgram :: (Monad m) => ALUProgram -> Array V DIM1 (Vector Int) -> m (Array U DIM1 ALUState)
runALUProgram aluProgram inputsArr =
    let initStates = R.fromFunction (R.extent inputsArr) (const (0, 0, 0, 0, 0))
        finalStates = 
            R.zipWith 
                (\initState input -> execState (runReaderT aluProgram input) initState)
                initStates
                inputsArr
    in R.computeUnboxedP finalStates

findLargestZ1Number :: Int -> Int -> ALUProgram -> IO (Maybe (Vector Int))
findLargestZ1Number batchSize numDigits aluProgram = do
    let numbersList = V.fromList <$> replicateM numDigits ([9,8..1] :: [Int])
    doFind (R.fromList (ix1 batchSize) <$> chunksOf batchSize numbersList)
    where 
        isZzero :: (a, ALUState) -> Bool
        isZzero (_, aluState) = (aluState ^?! ix z) == 0
        doFind inputBatches = do
            case inputBatches of
                (curBatch:rest) -> do
                    putStrLn $ "Computing one batch of extent: " ++ show (R.extent curBatch)
                    itStart <- getTime
                    batchOutputs <- runALUProgram aluProgram curBatch
                    itEnd <- batchOutputs `R.deepSeqArray` getTime
                    putStrLn $ "Computed batch in " ++ show (itEnd - itStart) ++ " seconds"
                    case find isZzero (R.toList $ R.zipWith (,) curBatch batchOutputs) of
                        Nothing -> doFind rest
                        Just (inputNumber, _) -> return $ Just inputNumber
                [] -> return Nothing

main :: IO ()
main = do
    initializeTime
    testAluProgram <- parseOrDie parseALUProgram <$> TIO.readFile "aoc21_day24_test"
    let testInputsArr = R.fromList (ix1 1) [V.singleton 15]
    testOutputs <- head . R.toList <$> runALUProgram testAluProgram testInputsArr
    assert (testOutputs == (1, 1, 1, 1, 1)) $ do
        aluProgram <- parseOrDie parseALUProgram <$> TIO.readFile "aoc21_day24"
        let checkInputsArr = R.fromList (ix1 1) [V.fromList $ replicate 14 9]
        checkOutput <- runALUProgram aluProgram checkInputsArr
        putStrLn $ "Check output: " ++ show checkOutput
        largestZ1Number <- findLargestZ1Number (1024^2) 14 aluProgram
        putStrLn $ "Question 1 answer is: " ++ show largestZ1Number

