{-# LANGUAGE ViewPatterns #-}
module AOC.Common.Algo where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL((:<)))

bfs :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfs repr step ss = go Set.empty (Seq.fromList ss)
    where go _ (Seq.viewl -> Seq.EmptyL) = []
          go seen (Seq.viewl -> h :< t) | repr h `Set.member` seen = go seen t
          go seen (Seq.viewl -> h :< t) = let nSeen = Set.insert (repr h) seen
                             in h : go nSeen (t <> (Seq.fromList $ step h))
          go _ _ = error "Unreachable"

traversal :: (a -> [a]) -> [a] -> [a]
traversal step ss = go (Seq.fromList ss)
    where go (Seq.viewl -> Seq.EmptyL) = []
          go (Seq.viewl -> h :< t) = h : go (t <> (Seq.fromList $ step h))
          go _ = error "Unreachable"

dfs :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
dfs repr step ss = ss >>= go Set.empty
  where
    go seen cs
      | Set.member (repr cs) seen = []
      | otherwise = cs : (step cs >>= go (Set.insert (repr cs) seen))

countRepeat :: (Ord a, Eq a) => [a] -> Int
countRepeat as = go 0 as Set.empty
    where go n (x:xs) seen | x `Set.member` seen = n
                           | otherwise           = go (succ n) xs (Set.insert x seen)
          go _ []     _                          = -1
