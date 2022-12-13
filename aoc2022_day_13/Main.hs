module Main where

import Data.Text.IO qualified as TIO
import Imports
import ParseUtils

data Elem = I Int | L [Elem]

-- LT: ordered, GT or EQ: not ordered
elemCompare :: Elem -> Elem -> Ordering
elemCompare (I i) (I j) = compare i j
elemCompare (L []) (L []) = EQ
elemCompare (L (i : is)) (L (j : js)) =
  case elemCompare i j of
    EQ -> elemCompare (L is) (L js)
    other -> other
elemCompare (L []) (L _) = LT
elemCompare (L _) (L []) = GT
elemCompare list (I i) = elemCompare list (L [I i])
elemCompare (I i) list = elemCompare (L [I i]) list

instance Eq Elem where
  e1 == ej = elemCompare e1 ej == EQ

instance Ord Elem where
  compare = elemCompare

parseElem :: Parsec Void Text Elem
parseElem =
  I <$> decimal <|> do
    void $ char '['
    listElems <- sepBy parseElem (char ',')
    void $ char ']'
    return $ L listElems

parsePairs :: Parsec Void Text [(Elem, Elem)]
parsePairs = some $ do
  l <- parseElem
  void newline
  r <- parseElem
  void newline
  void newline <|> eof
  return (l, r)

sumOrderedPairs :: [(Elem, Elem)] -> Int
sumOrderedPairs pairs = sum $ fmap fst $ filter (uncurry (<) . snd) $ zip [1 ..] pairs

decoderKey :: [(Elem, Elem)] -> Int
decoderKey pairs =
  let dividers@[d1, d2] =
        [ L [L [I 2]],
          L [L [I 6]]
        ]
      sortedElems = sort $ dividers ++ concat [[i, j] | (i, j) <- pairs]
   in (fromJust (elemIndex d1 sortedElems) + 1)
        * (fromJust (elemIndex d2 sortedElems) + 1)

main :: IO ()
main = do
  testPairs <- parseOrDie parsePairs <$> TIO.readFile "aoc22_day13_test"
  let actualSum = sumOrderedPairs testPairs
  assert (actualSum == 13) $ do
    pairs <- parseOrDie parsePairs <$> TIO.readFile "aoc22_day13_input"
    putStrLn $ "Question 1 answer is: " ++ show (sumOrderedPairs pairs)
    putStrLn $ "Question 2 answer is: " ++ show (decoderKey pairs)
