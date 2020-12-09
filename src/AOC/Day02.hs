module AOC.Day02 where

import AOC.Common.Prelude

data Policy = Policy (Int, Int) Char String
  deriving (Show)

type Input = [Policy]

parser :: Parser Input
parser =
  let policy =
        Policy
        <$> ((,) <$> natural <* symbol "-" <*> natural)
        <*> letterChar
        <*  symbol ":"
        <*> many letterChar
  in
  many (policy <* newline)

one :: Input -> Int
one xs =
  let isValid (Policy (mi, ma) c cs) =
        let l = length . filter (==c) $ cs
        in l >= mi && l <= ma
  in
  length . filter isValid $ xs

two :: Input -> Int
two xs =
  let
    isValid (Policy (mi, ma) c cs) = (cs !! (mi - 1) == c)
                                  /= (cs !! (ma - 1) == c)
  in
  length . filter isValid $ xs
  
tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day02.txt"

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
