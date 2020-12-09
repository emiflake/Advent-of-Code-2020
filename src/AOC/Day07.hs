module AOC.Day07 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe

import Data.Foldable (asum)

type Bag = String

data Rule = Contains Bag [(Int, Bag)]
  deriving (Show, Eq)

type Input = [Rule]

plurality :: Parser Text -> Parser Text
plurality p =
  p <> (try (symbol "s") <|> pure "")

parser :: Parser Input
parser = many rule
  where
    color = some letterChar <> (sc >> pure " ") <> some letterChar

    bag = plurality "bag"

    contents =
      asum
      [ lexeme "no other bags" *> pure []
      , ((,) <$> natural <* sc <*> color <* sc <* bag) `sepBy` (symbol ",")
      ]

    rule =
      Contains
      <$> lexeme color
      <*  lexeme "bags"
      <*  lexeme "contain"
      <*> contents
      <*  symbol "."

transitiveContains :: Bag -> [Rule] -> [Bag]
transitiveContains needle rules =
  bfs id
  (\bag ->
      let
        sub = filter (\(Contains _ sub) -> any ((==bag) . snd) sub) rules
      in fmap (\(Contains b _) -> b) sub)
    [needle]

subbags :: Bag -> [Rule] -> [(Int, Bag)]
subbags needle rules =
  traversal
  (\(c, bag) ->
     let Contains _ sub = fromJust $ find (\(Contains b _) -> b == bag) rules
     in (\(sc,sb) -> (sc * c, sb)) <$> sub
  )
  [(1, needle)]

one :: Input -> _
one = length . tail . nub . transitiveContains "shiny gold"

two :: Input -> _
two = sum . fmap fst . tail . subbags "shiny gold"

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day07.txt"

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
