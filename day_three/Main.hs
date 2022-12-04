module Main where

import Control.Exception
import Data.List.Split
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void
import ParseUtils
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char

ruckasckToCompartments :: [Char] -> (Set Char, Set Char)
ruckasckToCompartments rucksack =
  let rucksackSize = length rucksack
      (c1List, c2List) = assert (even rucksackSize) $ splitAt (rucksackSize `div` 2) rucksack
   in (Set.fromList c1List, Set.fromList c2List)

rucksackParser :: ([Char] -> a) -> Parsec Void Text [a]
rucksackParser convert = many $ do
  compartments <- convert <$> many letterChar
  newline
  return compartments

compartmentShared :: (Set Char, Set Char) -> Char
compartmentShared (c1, c2) =
  let commonItems = Set.intersection c1 c2
   in assert (length commonItems == 1) $ Set.findMax commonItems

badge :: [Set Char] -> Char
badge group =
  let commonItems = foldr1 Set.intersection group
   in assert (length commonItems == 1) $ Set.findMax commonItems

itemPriority :: Char -> Integer
itemPriority item =
  let itemIdx = fromEnum item
   in fromIntegral $
        if fromEnum 'a' <= itemIdx && itemIdx <= fromEnum 'z'
          then itemIdx - fromEnum 'a' + 1
          else itemIdx - fromEnum 'A' + 27

main :: IO ()
main = do
  inputContents <- TIO.readFile "input_3"
  let compartmentList = parseOrDie (rucksackParser ruckasckToCompartments) inputContents
  putStrLn $ "Solution to question 1 is: " ++ show (sum $ fmap (itemPriority . compartmentShared) compartmentList)
  let rucksackList = parseOrDie (rucksackParser Set.fromList) inputContents
      groupList = chunksOf 3 rucksackList
  putStrLn $ "Solution to question 2 is: " ++ show (sum $ fmap (itemPriority . badge) groupList)
