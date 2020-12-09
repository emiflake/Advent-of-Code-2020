module AOC.Day09 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe

type Input = [Int]

parser :: Parser Input
parser = many integer

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

see = fibs !! 18783 + fibs !! 18785

one :: Input -> _
one =
    last
  . head
  . dropWhile findPair
  . filter ((==26) . length)
  . fmap (take 26)
  . tails
  where
    findPair xs = or $ (\a b -> a /= b && a + b == last xs) <$> (init xs) <*> (init xs)

window :: Int -> [a] -> [[a]]
window size = filter ((== size) . length) . map (take size) . tails

two :: Input -> _
two xs =
  let
    needle = one xs
    sublist =
        fromJust
      . find ((==needle) . sum)
      $ (`window` xs) =<< [2..length xs]
  in
  (+) <$> maximum <*> minimum $ sublist


tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day09.txt"

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
