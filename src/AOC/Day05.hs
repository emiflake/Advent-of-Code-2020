module AOC.Day05 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import Control.Monad

type Input = [String]

parser :: Parser Input
parser = many (many letterChar <* newline)

binaryReduce :: Int -> [Bool] -> Int
binaryReduce _ [] = 0
binaryReduce n (True:xs) = (n `div` 2) + binaryReduce (n `div` 2) xs
binaryReduce n (False:xs) = binaryReduce (n `div` 2) xs

getID :: String -> Int
getID xs = let x = binaryReduce 128 (fmap (=='B') (take 7 xs))
               y = binaryReduce 8   (fmap (=='R') (drop 7 xs))
           in y + x * 8

one :: Input -> Int
one =
  getID . maximumBy (comparing getID)

two :: Input -> Int
two xs =
  let ids = nub $ fmap getID xs
  in
  fromJust
  . find (\e -> e `notElem` ids)
  . dropWhile (\e -> e `notElem` ids)
  $ [0..(8+128*8)]

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day05.txt"

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
