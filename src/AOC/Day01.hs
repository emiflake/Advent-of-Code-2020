module AOC.Day01 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe

type Input = [Int]

parser :: Parser Input
parser = many integer

one :: Input -> Int
one xs =
    fromMaybe 0
  . fmap (\(a, b) -> a * b)
  . find (\(a, b) -> a + b == 2020)
  $ (,) <$> xs <*> xs

two :: Input -> Int
two xs =
    fromMaybe 0
  . fmap (\(a, b, c) -> a * b * c)
  . find (\(a, b, c) -> a + b + c == 2020)
  $ (,,) <$> xs <*> xs <*> xs

tests :: SpecWith ()
tests = do
  let exampleInput = [1721, 979, 366, 299, 675, 1456]

  describe "examples" $ do
    it "should solve example part 1" $ do
      one exampleInput `shouldBe` 514579
    it "should solve example part 2" $ do
      two exampleInput `shouldBe` 241861950

inputFile :: FilePath
inputFile = "inputs/day01.txt"

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
