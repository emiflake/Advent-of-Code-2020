{-# LANGUAGE BangPatterns #-}
module AOC.Day15 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Debug.Trace

type Input = [Int]

parser :: Parser Input
parser = integer `sepBy` (symbol ",")


step :: (IntMap Int, Int, Int) -> (IntMap Int, Int, Int)
step (seen, last, time) =
      case seen IntMap.!? last of
        Nothing -> (IntMap.insert last time seen,        0, time + 1)
        Just x ->  (IntMap.insert last time seen, time - x, time + 1)

one :: Input -> _
one xs =
  list
  where
        accum = iterate step (IntMap.fromList (zip xs [0..(length xs - 2)]), last xs, length xs - 1)

        (_, list, _) = accum !! (2020 - length xs)

applyN 0 f x = x
applyN n f !x = applyN (n - 1) f (f x)

two :: Input -> _
two xs =
  list
  where
        (_, list, _) = applyN (30000000 - length xs) step (IntMap.fromList (zip xs [0..(length xs - 2)]), last xs, length xs - 1)
tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day15.txt"

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
