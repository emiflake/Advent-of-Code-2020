module AOC.Day01 where

import AOC.Common.Prelude

type Input = [Int]

parser :: Parser Input
parser = integer `sepBy` symbol ","

one :: Input -> IO Int
one = pure . sum

two :: Input -> IO Int
two = pure . product

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day01.txt"

commands :: [(Text, IO ())]
commands =
  []

today :: Day [Int] Int Int
today =
  day
    parser
    one
    two
    tests
    inputFile
    commands
