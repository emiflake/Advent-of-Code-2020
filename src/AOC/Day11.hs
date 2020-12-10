module AOC.Day11 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe

type Input = [Int]

parser :: Parser Input
parser = many integer

one :: Input -> _
one xs = sum xs

two :: Input -> _
two xs = product xs

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day11.txt"

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
