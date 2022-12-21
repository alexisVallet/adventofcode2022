{- 
Attempt at a somewhat efficient N-dimensional pareto front data structure.
-}
module ParetoFront where

import Data.IntMap.Strict qualified as IMap
import Imports

type Element = [Int]

newtype ParetoFront a = PF {
    unPF :: Either (IntMap a) (IntMap (ParetoFront a))
} deriving (Generic)

instance Functor ParetoFront where
    fmap f (PF (Left leafMap)) = PF (Left (fmap f leafMap))
    fmap f (PF (Right nodeMap)) = PF (Right (fmap (fmap f) nodeMap))

instance Foldable ParetoFront where
    foldr f b (PF (Left leafMap)) = foldr f b leafMap
    foldr f b (PF (Right nodeMap)) = foldr (flip (foldr f)) b nodeMap

splitLevel :: Int -> IntMap a -> (IntMap a, IntMap a)
splitLevel i imap =
    let (lower, mEq, greater) = IMap.splitLookup i imap
        tryInsert submap = case mEq of
            Nothing -> submap
            Just a -> IMap.insert i a submap
    in (tryInsert lower, tryInsert greater)

iMapUnzip :: IntMap (a, a) -> (IntMap a, IntMap a)
iMapUnzip map =
    let (keys, a1s, a2s) = unzip3 $ (\(k, (a1, a2)) -> (k, a1, a2)) <$> IMap.assocs map
    in
        (IMap.fromDistinctAscList $ zip keys a1s,
        IMap.fromDistinctAscList $ zip keys a2s)

-- Based on a (potentially new) element, splits a pareto front into
-- a (dominated, dominant) elements in a divide and conquer
-- fashion. Non comparable elements are ignored.
-- Unless what I am doing is extremely wrong. This should be O(d log(N)^2)
-- making some mild assumptions about the distribution of elements.
splitPareto :: Element -> ParetoFront a -> (ParetoFront a, ParetoFront a)
splitPareto [] _ = error "Element is not large enough!"
splitPareto [_] (PF (Right _)) = error "Element is not large enough!"
-- At the leaf node level, we have a total order, so no elements are
-- incomparable.
splitPareto [i] (PF (Left leafMap)) = do
    let (lowerEq, greaterEq) = splitLevel i leafMap
     in (PF (Left lowerEq), PF (Left greaterEq))
splitPareto (_:_) (PF (Left _)) = error "Element is too large!"
-- At the node level, we split and call recursively on both the lowerEq
-- and greaterEq side. The lowerEq -> lowerEq case are dominated, the
-- greaterEq -> greaterEq are dominant.
-- The remaining elements are non comparable, so we ignore them.
splitPareto (i:is) (PF (Right nodeMap)) =
    let (lowerEq, greaterEq) = splitLevel i nodeMap
        (lowerDominated, _) = iMapUnzip $ fmap (splitPareto is) lowerEq
        (_, greaterDominant) = iMapUnzip $ fmap (splitPareto is) greaterEq
     in (PF (Right lowerDominated), PF (Right greaterDominant))

frontLookup :: Element -> ParetoFront a -> Maybe a
frontLookup [i] (PF (Left leafMap)) = IMap.lookup i leafMap
frontLookup (i:is) (PF (Right nodeMap)) = do
    subMap <- IMap.lookup i nodeMap
    frontLookup is subMap
frontLookup _ _ = error "The element is either too large ot too small for the front!"

frontDifference :: ParetoFront a -> ParetoFront a -> ParetoFront a
frontDifference (PF (Left leafMap1)) (PF (Left leafMap2)) = PF (Left (IMap.difference leafMap1 leafMap2))
frontDifference (PF (Right nodeMap1)) (PF (Right nodeMap2)) =
    PF (Right $ IMap.differenceWithKey recurseDiff nodeMap1 nodeMap2)
    where recurseDiff _ m1 m2 = 
            let diffOut = frontDifference m1 m2
             in if null diffOut then Nothing else Just diffOut
frontDifference _ _ = error "fronts do not have the same depth!"

frontInsert :: Element -> a -> ParetoFront a -> ParetoFront a
frontInsert [i] a (PF (Left leafMap)) = PF (Left $ IMap.insert i a leafMap)
frontInsert (i:is) a (PF (Right nodeMap)) = PF (Right $ IMap.alter (Just . maybe (frontSingleton is a) (frontInsert is a)) i nodeMap)
frontInsert _ _ _ = error "The element is either too large or too small for the front!"

frontSingleton :: Element -> a -> ParetoFront a
frontSingleton [i] a = PF (Left $ IMap.singleton i a)
frontSingleton (i:is) a = PF (Right $ IMap.singleton i (frontSingleton is a))
frontSingleton _ _ = error "Element cannot be an empty list!"

-- Fused memoizing and pareto front maintenance.
-- Returns (mVal, newFront, isDominated) where:
-- * mVal is the result of looking up the element. Nothing if the element is new,
--   or yet to be computed. Just x if the value has been computed.
-- * newFront is the front updated with the new value if the new value is not dominated.
-- * isDominated is true if the value is dominated by any value in the pareto front.
insertLookup :: Element -> ParetoFront (Maybe a) -> (Maybe a, ParetoFront (Maybe a), Bool)
insertLookup elem front =
    let lookupRes = join $ frontLookup elem front
        (newFront, isDominated) = case lookupRes of
            Just _ -> (front, False)
            Nothing ->
                let (dominated, dominating) = splitPareto elem front
                in if not (null dominating) then
                    -- if elem is dominated by any element, return the pareto front
                    -- unchanged as it should not be inserted.
                    (front, True)
                   else if not (null dominated) then
                    -- if elem is dominating any element. We remove all dominated
                    -- elements, and insert the new element in the front.
                    (frontInsert elem Nothing $ frontDifference front dominated, False)
                   else
                    -- Otherwise, elem is not comparable to existing elements in the
                    -- pareto front, so we simply add it.
                    (frontInsert elem Nothing front, False)
     in (lookupRes, newFront, isDominated)
