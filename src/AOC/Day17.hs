{-# LANGUAGE BangPatterns #-}
module AOC.Day17 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Linear

-- type Input = WorldState
type Input = [[Bool]]

type WorldState = Set (V3 Int)

toSet3 :: [[Bool]] -> Set (V3 Int)
toSet3 input =
  Set.fromList $
  (\(row, xs) -> mapMaybe (\(col, v) ->
    if v then Just (V3 row col 0) else Nothing) $ zip [0..] xs)
  =<< zip [0..] input

promote :: Set (V3 Int) -> Set (V4 Int)
promote = Set.fromList . fmap (\(V3 x y z) -> V4 x y z 0) . Set.toList

parser :: Parser Input
parser =
  many (some cell <* newline)
  where cell = (char '#' >> pure True) <|> (char '.' >> pure False)

neighbours3 :: V3 Int -> [V3 Int]
neighbours3 root =
  fmap (^+^ root) . filter (/=(V3 0 0 0)) $ V3 <$> [(-1)..1] <*> [(-1)..1] <*> [(-1)..1]

neighbours4 :: V4 Int -> [V4 Int]
neighbours4 root =
    fmap (^+^ root)
  . filter (/=(V4 0 0 0 0))
  $ V4
  <$> [(-1)..1]
  <*> [(-1)..1]
  <*> [(-1)..1]
  <*> [(-1)..1]

next :: Ord a => (a -> [a]) -> Set a -> Set a
next neighbours !xs =
    Set.filter step
  . Set.union xs
  . Set.fromList
  . concatMap neighbours
  . Set.toList
  $ xs
  where
    countNeighbours = Set.size . Set.fromList . filter (`Set.member` xs) . neighbours
    step p
      | p `Set.member` xs && countNeighbours p `elem` [2, 3] = True
      | not (p `Set.member` xs) && countNeighbours p == 3    = True
      | otherwise = False

one :: Input -> _
one = length . (!!6) . iterate (next neighbours3) . toSet3

two :: Input -> _
two = length . (!!6) . iterate (next neighbours4) . promote . toSet3

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day17.txt"

commands :: [(Text, IO ())]
commands =
  []

today :: Day Input _ _
today =
  day
    parser
    (pure . one)
    (pure . two)
    tests
    inputFile
    commands
