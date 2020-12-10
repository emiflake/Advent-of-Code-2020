module AOC.Day10 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import qualified Data.Array as Array
import Data.Array (Array, (!))
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace

type Input = [Int]

parser :: Parser Input
parser = many integer

next :: Array Int Int -> Map Int [Int]
next xs =
  let (low, high) =
        Array.bounds xs
  in
    Map.fromList
  $ fmap
  (\i ->
      ( i
      , [ i + o
        | o <- [1..3]
        , i + o <= high
        , let (a, b) = (xs ! i, xs ! (i + o))
        , b > a && b <= a + 3
        ] )
  )
  [low..high]

one :: Input -> _
one xs =
  let
    joltPath = sort xs
    differences = fmap abs $ zipWith (-) joltPath (tail joltPath)
  in
    ((length . filter (==3) $ differences) + 1)
  * ((length . filter (==1) $ differences) + 1)

two :: Input -> _
two xs =
  let
    ordering = sort xs
    arr = Array.listArray (0, length ordering - 1) ordering
    optionsFrom =
      Map.mapWithKey
      (\k jmps ->
         case jmps of
           [] -> 1
           _ -> sum $ fmap (optionsFrom Map.!) jmps
      )
      (next arr)
  in
  sum $ (optionsFrom Map.!) <$> [0..2]


tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day10.txt"

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
