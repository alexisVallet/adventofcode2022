module Main where

import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text.IO qualified as TIO
import Imports hiding (Empty)
import ParseUtils

encryptedFileParser :: Parsec Void Text (Seq Int)
encryptedFileParser = fmap Seq.fromList $
  some $ do
    n <- signed space decimal
    newline
    return n

data Zipper a = Zipper
  { left :: Seq a,
    center :: a,
    right :: Seq a
  }
  deriving (Generic)

zipperAt :: Int -> Seq a -> Zipper a
zipperAt idx seq =
  let (left, center :<| right) = Seq.splitAt idx seq
   in Zipper left center right

-- So the rules are weird. We wrap around not when there are 0
-- elements on the list we are consuming, but when there is 1.
-- Which is not consistent with what happens when we do move off
-- the last element on the list to wrap around.
shiftRight :: Zipper a -> Zipper a
shiftRight (Zipper ls c Empty) = shiftRight (Zipper Empty c ls)
shiftRight (Zipper ls c (r :<| Empty)) = Zipper Empty c (ls :|> r)
shiftRight (Zipper ls c (r :<| rs)) = Zipper (ls :|> r) c rs

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Zipper Empty c rs) = shiftLeft (Zipper rs c Empty)
shiftLeft (Zipper (Empty :|> l) c rs) = Zipper (l :<| rs) c Empty
shiftLeft (Zipper (ls :|> l) c rs) = Zipper ls c (l :<| rs)

zipperSeq :: Zipper a -> Seq a
zipperSeq (Zipper ls c rs) = (ls :|> c) <> rs

mixAt :: Int -> Seq (Int, Int) -> Seq (Int, Int)
mixAt origIdx numbers =
  let idx = fromJust $ Seq.findIndexL ((== origIdx) . fst) numbers
      z@(Zipper _ (_, c) _) = zipperAt idx numbers
      shift = if c < 0 then shiftLeft else shiftRight
      rawShiftSize = abs c
      n = Seq.length numbers - 1
      shiftSize =
        if rawShiftSize < n
          then rawShiftSize
          else rawShiftSize `mod` n
   in zipperSeq $ foldr (const shift) z [1 .. shiftSize]

mixNumbers :: [Int] -> Seq (Int, Int) -> Seq (Int, Int)
mixNumbers indices zippedNumbers = foldl (flip mixAt) zippedNumbers indices

groveCoordsSum :: Int -> Seq Int -> Int
groveCoordsSum numRounds numbers =
  let indices = [0 .. Seq.length numbers - 1]
      zippedNumbers = Seq.zip (Seq.fromList indices) numbers
      mixed = snd <$> foldr (const $ mixNumbers indices) zippedNumbers [1 .. numRounds]
      n = Seq.length mixed
      zeroPos = fromJust $ Seq.findIndexL (== 0) mixed
      wrapIdx i = (zeroPos + i) `rem` n
   in mixed ^?! ix (wrapIdx 1000) + mixed ^?! ix (wrapIdx 2000) + mixed ^?! ix (wrapIdx 3000)

main :: IO ()
main = do
  testNumbers <- parseOrDie encryptedFileParser <$> TIO.readFile "aoc22_day20_test"
  let actualGrove = groveCoordsSum 1 testNumbers
  assert (actualGrove == 3) $ do
    numbers <- parseOrDie encryptedFileParser <$> TIO.readFile "aoc22_day20_input"
    putStrLn $ "Question 1 answer is: " ++ show (groveCoordsSum 1 numbers)
    let numbers' = fmap (* 811589153) numbers
    putStrLn $ "Question 2 answer is: " ++ show (groveCoordsSum 10 numbers')
