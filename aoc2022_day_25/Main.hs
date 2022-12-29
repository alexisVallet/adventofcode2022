module Main (main) where

import Data.Foldable
import Data.Text.IO qualified as TIO
import Imports as I
import ParseUtils
import Z3.Monad as Z3

type Snafu = [Int]

snafuToInt :: Snafu -> Int
snafuToInt snafu = foldr (\(power, digit) rest -> digit * 5 ^ power + rest) 0 $ zip ([length snafu - 1, length snafu -2 ..] :: [Int]) snafu

snafuToIntZ3 :: [AST] -> Z3 AST
snafuToIntZ3 snafuAST = do
  five <- mkInteger 5
  one <- mkInteger 1
  zero <- mkInteger 0
  (intAst, _) <-
    foldrM
      ( \digit (rest, powerOf5) -> do
          digitVal <- mkMul [digit, powerOf5]
          newRestVal <- mkAdd [digitVal, rest]
          newPowerOf5 <- mkMul [five, powerOf5]
          return (newRestVal, newPowerOf5)
      )
      (zero, one)
      snafuAST
  return intAst

intToSnafuZ3 :: Int -> Int -> Z3 Snafu
intToSnafuZ3 numDigits i = do
  two <- mkInteger 2
  minusTwo <- mkInteger (-2)
  snafuAST <- forM [1 .. numDigits] $ \i -> do
    digitVar <- mkFreshIntVar $ show i
    mkLe minusTwo digitVar >>= Z3.assert
    mkLe digitVar two >>= Z3.assert
    return digitVar
  intAST <- snafuToIntZ3 snafuAST
  iAST <- mkInteger $ fromIntegral i
  eqAST <- mkEq intAST iAST
  Z3.assert eqAST
  mSnafuIntegers <- fmap (join . snd) $ withModel $ \m -> mapEval evalInt m snafuAST
  return $ fromIntegral <$> fromJust mSnafuIntegers

snafuParser :: Parsec Void Text [Snafu]
snafuParser = some $ do
  snafu <- some $ do
    digit <- oneOf ("210-=" :: [Char])
    return $ case digit of
      '-' -> -1
      '=' -> -2
      _ -> read [digit]
  newline
  return snafu

showSnafu :: Snafu -> String
showSnafu =
  dropWhile (== '0')
    . fmap
      ( \i -> case i of
          2 -> '2'
          1 -> '1'
          0 -> '0'
          -1 -> '-'
          -2 -> '='
          _ -> error $ "Invalid snafu digit " ++ show i
      )

main :: IO ()
main = do
  testSnafus <- parseOrDie snafuParser <$> TIO.readFile "aoc22_day25_test"
  let actualSum = sum $ snafuToInt <$> testSnafus
  I.assert (actualSum == 4890) $ return ()
  actualSumSnafu <- fmap showSnafu $ evalZ3 $ intToSnafuZ3 12 actualSum
  I.assert (actualSumSnafu == "2=-1=0") $ return ()
  snafus <- parseOrDie snafuParser <$> TIO.readFile "aoc22_day25_input"
  sumSnafu <- fmap showSnafu $ evalZ3 $ intToSnafuZ3 30 $ sum $ snafuToInt <$> snafus
  putStrLn $ "Question 1 answer is: " ++ sumSnafu
