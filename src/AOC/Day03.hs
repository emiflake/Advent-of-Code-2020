module AOC.Day03 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe

type Input = [[Char]]

parser :: Parser Input
parser = many (many printChar <* newline)

checkslope :: Input -> (Int, Int) -> Int
checkslope xs (dx, dy) =
  let yl = length xs `div` dy
      xl = length (head xs)
  in
    length
  . filter (\(x, y) -> (xs !! y) !! (x `mod` xl) == '#')
  . take yl
  . fmap (\i -> (dx * i, dy * i))
  $ [0..]

one :: Input -> Int
one xs =
  checkslope xs (3, 1)

two :: Input -> Int
two xs = do
  product $ fmap (checkslope xs) [ (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) ]

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day03.txt"

commands :: [(Text, IO ())]
commands =
  []

today :: Day Input Int Int
today =
  day
    parser
    (pure . one)
    (pure . two)
    tests
    inputFile
    commands
