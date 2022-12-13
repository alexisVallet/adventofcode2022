module IntCode where

import Imports

import Control.Monad.Coroutine
import Data.Vector.Unboxed hiding ((++), find)
import Data.Vector.Unboxed qualified as V

type IntCodeProgram = Vector Int

intCodeParser :: Parsec Void Text IntCodeProgram
intCodeParser = fromList <$> sepBy decimal (char ',')

data IntCodeState = IntCodeState {
    program :: IntCodeProgram,
    curPos :: Int
} deriving (Generic, Show)

type IntCode = State IntCodeState


binOp :: (Int -> Int -> Int) -> IntCode ()
binOp op = do
    curPos' <- use #curPos
    program' <- use #program
    let in1 = program' ^?! ix (program' ^?! ix (curPos' + 1))
        in2 = program' ^?! ix (program' ^?! ix (curPos' + 2))
        outLoc = program' ^?! ix (curPos' + 3)
    #program . ix outLoc .= op in1 in2
    #curPos += 4
    runIntCode

runIntCode :: IntCode ()
runIntCode = do
    curPos' <- use #curPos
    curOpCode <- fromJust <$> preuse (#program . ix curPos')
    case curOpCode of
        1 -> do
            binOp (+)
        2 -> do
            binOp (*)
        99 -> return ()
        _ -> error $ "Invalid opcode " ++ show curOpCode

initState :: IntCodeProgram -> IntCodeState
initState p = IntCodeState {
    program=p, curPos=0
}

execIntCode ::  IntCodeProgram -> Int -> Int-> IntCodeProgram
execIntCode p noun verb = 
    program $ execState runIntCode $ initState $ 
        p & ix 1 .~ noun & ix 2 .~ verb
