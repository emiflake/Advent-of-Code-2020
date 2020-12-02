module AOC.Day03 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe

type Input = [Int]

parser :: Parser Input
parser = many integer

one :: Input -> Int
one xs = sum xs

two :: Input -> Int
two xs = product xs

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day03.txt"

commands :: [(Text, IO ())]
commands =
  []

today :: Day [Int] Int Int
today =
  day
    parser
    (pure . one)
    (pure . two)
    tests
    inputFile
    commands
