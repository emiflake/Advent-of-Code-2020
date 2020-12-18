module AOC.Day16 where

import AOC.Common.Prelude

import Data.List
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Functor
import Control.Monad
import Data.Foldable

data Range = Range Integer Integer
  deriving (Show, Eq)

data Info
  = Info
  { fieldRanges :: [(Text, [Range])]
  , myTicket :: [Integer]
  , nearbyTickets :: [[Integer]]
  }
  deriving (Show, Eq)

type Input = Info

-- Don't ask me why I needed to make this. Oh dear.
replicateWhile p f = f >>= \v -> if p v then fmap (v:) (replicateWhile p f) else pure []

parser :: Parser Input
parser =
  Info
  <$> some (try fieldRange)
  <* sc
  <*> ("your ticket" *> symbol ":" *> nats)
  <*> ("nearby tickets" *> symbol ":" *> (replicateWhile ((>0) . length) nats))
  where
    nats = natural `sepBy` symbol ","

    myTicket = "your ticket" *> symbol ":" *> natural `sepBy` symbol ","

    range =
      Range <$> natural <* symbol "-" <*> natural

    word = fmap (Text.pack . unwords) $ some (lexeme (some letterChar))

    fieldRange :: Parser (Text, [Range])
    fieldRange =
      (,) <$> word <* symbol ":" <*> (range `sepBy` lexeme "or")


validAny :: Integer -> [Range] -> Bool
validAny n = any (\(Range min max) -> n >= min && n <= max)

one :: Input -> _
one (Info ranges my neighbour) =
  sum . filter (not . (`validAny` (ranges >>= snd))) . concat $ neighbour


two :: Input -> _
two (Info ranges my neighbour) =
  let validNeighbours = filter (all (`validAny` (ranges >>= snd))) $ neighbour

      pairs ranges fieldLists =
        [ (fieldName, col)
        | (fieldName, range) <- ranges
        , (col, fieldList) <- zip [0..] fieldLists
        , all (`validAny` range) fieldList
        ]

      ps = pairs ranges (transpose validNeighbours)

      colLookup =
        [ (col, fmap fst $ filter ((==col) . snd) ps)
        | col <- [0..length ranges - 1]
        ]

      fieldLookup =
        [ (field, fmap snd $ filter ((==field) . fst) ps)
        | field <- fmap fst ranges
        ]

      go [] acc = acc
      go fieldCols acc =
        case find ((==1) . length . snd) fieldCols of
          Just (field, [col]) ->
            go
            (fmap (\(a, b) -> (a, delete col b)) $ filter ((/=field) . fst) fieldCols)
            ((field,col):acc)
          _ -> error "should not happen"
  in
    product
  . fmap snd
  . fmap (\(field, col) -> (field, my !! col))
  . fmap (\(a, b) -> (b, a))
  . filter (isPrefixOf "departure" . Text.unpack . snd)
  $ go colLookup []



tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day16.txt"

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
