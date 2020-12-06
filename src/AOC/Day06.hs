module AOC.Day06 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import qualified Data.Set as Set

type Input = [[[Char]]]

parser :: Parser Input
parser = group `sepBy` newline
  where group = many (some letterChar <* newline)

one :: Input -> Int
one =
  sum . fmap (length . nub . concat)

two :: Input -> Int
two =
  sum . fmap (Set.size . foldl1 Set.intersection . fmap Set.fromList)

tests :: SpecWith ()
tests =
  describe "examples" $ do
    let input =
          [ [ "abc" ]
          , [ "a" , "b" , "c"]
          , [ "ab" , "ac"]
          , [ "a" , "a" , "a" , "a"]
          , [ "b" ]
          ]
    it "should pass part 1" $ do
      one input `shouldBe` 11
    it "should pass part 2" $ do
      two input `shouldBe` 6

inputFile :: FilePath
inputFile = "inputs/day06.txt"

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
