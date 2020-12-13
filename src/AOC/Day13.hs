{-# LANGUAGE TupleSections #-}
module AOC.Day13 where

import AOC.Common.Prelude

import Data.List
import Data.Ord
import Data.Maybe


type Input = (Integer, [Maybe Integer])

parser :: Parser Input
parser = (,)
  <$> integer
  <*> (Just <$> natural <|> ("x" *> pure Nothing)) `sepBy` ","

one :: Input -> _
one (start, busses) =
    uncurry (*)
  . minimumBy (comparing snd)
  . fmap (\b -> (b, b - start `mod` b))
  . catMaybes
  $ busses

bezout :: (Integral a, Num a) => a -> a -> (a, a)
bezout 0 _ = (0, 1)
bezout a b =
  let (x, y) = bezout (b `mod` a) a
  in (y - (b `div` a) * x, x)

crt :: (Integral a, Num a) => [(a, a)] -> a
crt [] = 0
crt [(a, n)] = mod a n
crt ((a0, n0) : (a1, n1) : xs) =
  let (m0, m1) = bezout n0 n1
      a01 = a0 * m1 * n1 + a1 * m0 * n0
  in crt ((a01, n0 * n1) : xs)

two :: Input -> _
two =
    crt
  . mapMaybe (\(i, b) -> (-i,) <$> b)
  . zip [0..]
  . snd

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day13.txt"

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
