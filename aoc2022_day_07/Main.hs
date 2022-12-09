{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception
import Control.Lens hiding (uncons)
import Control.Monad.State.Strict
import Data.Either
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (HasField)
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void
import Debug.Trace
import GHC.Generics
import ParseUtils
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Pretty.Simple (pPrint)

type Directory = Map String DirOrFile

data DirOrFile = D Directory | F File
  deriving (Generic, Show)

dirOrFileToEither :: DirOrFile -> Either Directory File
dirOrFileToEither (D dir) = Left dir
dirOrFileToEither (F file) = Right file

_D :: Prism' DirOrFile Directory
_D =
  prism'
    D
    ( \d -> case d of
        D dir -> Just dir
        F _ -> Nothing
    )

_F :: Prism' DirOrFile File
_F =
  prism'
    F
    ( \d -> case d of
        D _ -> Nothing
        F f -> Just f
    )

newtype File = File
  { size :: Int
  }
  deriving (Generic, Show)

data ParserState = ParserState
  { curDir :: [String],
    curFS :: Directory
  }
  deriving (Generic, Show)

type FSParser = ParsecT Void Text (State ParserState)

initParserState :: ParserState
initParserState =
  ParserState
    { curDir = [],
      curFS = Map.singleton "/" (D mempty)
    }

dirLens :: [String] -> Traversal' ParserState Directory
dirLens = foldr accLens #curFS
  where
    accLens fname curLens = curLens . at fname . _Just . _D

parseCd :: FSParser ()
parseCd = do
  _ <- string "$ cd "
  dirNameOrParent <- do
    Right <$> (string ".." >> void newline) <|> Left <$> manyTill anySingle newline
  case dirNameOrParent of
    Left dirname -> do
      #curDir %= (dirname :)
    Right () -> #curDir %= snd . fromJust . uncons

addDirOrFile :: String -> DirOrFile -> FSParser ()
addDirOrFile dirname dirOrFile = do
  curDirVal <- use #curDir
  dirLens curDirVal . at dirname ?= dirOrFile

parseDir :: FSParser ()
parseDir = do
  void $ string "dir "
  dirname <- manyTill anySingle newline
  addDirOrFile dirname $ D mempty

parseFile :: FSParser ()
parseFile = do
  size <- decimal
  hspace
  fname <- manyTill anySingle newline
  addDirOrFile fname $ F $ File size

parseLs :: FSParser ()
parseLs = do
  _ <- string "$ ls\n"
  void $
    some $ do
      parseDir <|> parseFile

fsParser :: FSParser Directory
fsParser = do
  _ <- some $ do
    parseCd <|> parseLs
  use #curFS

parseFsOrDie :: Text -> Directory
parseFsOrDie input =
  case evalState (runParserT fsParser "" input) initParserState of
    Left err -> error $ errorBundlePretty err
    Right dir -> dir

dirSizes :: Directory -> [Int]
dirSizes dir =
  let (childDirs, files) = partitionEithers $ dirOrFileToEither <$> Map.elems dir
      childDirSizes = dirSizes <$> childDirs
   in sum (size <$> files) + sum (head <$> childDirSizes) : concat childDirSizes

sumOfDirsSmallerThan :: Int -> Directory -> Int
sumOfDirsSmallerThan thresh dir =
  let sortedSizes = sort $ dirSizes dir
   in sum $ takeWhile (< thresh) sortedSizes

sizeOfDirToDelete :: Int -> Directory -> Int
sizeOfDirToDelete necessaryAmount dir =
  let sizes = dirSizes dir
      unused = 70000000 - head sizes
      (_, large) = partition (< necessaryAmount - unused) sizes
   in minimum large

main :: IO ()
main = do
  testDir <- parseFsOrDie <$> TIO.readFile "aoc22_day07_input_test"
  let actualSumOfDirs = sumOfDirsSmallerThan 100000 testDir
      actualSizeToDelete = sizeOfDirToDelete 30000000 testDir
  assert (actualSumOfDirs == 95437 && actualSizeToDelete == 24933642) $ do
    dir <- parseFsOrDie <$> TIO.readFile "aoc22_day07_input"
    putStrLn $ "Question 1 answer is " ++ show (sumOfDirsSmallerThan 100000 dir)
    putStrLn $ "Question 2 answer is " ++ show (sizeOfDirToDelete 30000000 dir)
