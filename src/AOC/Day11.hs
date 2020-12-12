{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module AOC.Day11 where

import AOC.Common.Prelude

import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Functor
import Data.Function
import Linear hiding (project)
import Debug.Trace
import Prettyprinter
import Control.Lens (view)
import qualified Data.List.Split as Split


data Cell = Occupied | Empty | Floor
  deriving (Show, Eq)

-- Pretty printing
instance Pretty Cell where
  pretty = \case
    Occupied -> "#"
    Empty -> "L"
    Floor -> "."

instance Pretty c => Pretty (Map (V2 Int) c) where
  pretty m =
    let w = view _x . maximum . Map.keys $ m
    in
    Map.toList m
    & sortBy (comparing fst)
    & fmap snd
    & Split.chunksOf (w + 1)
    & fmap (sep . fmap pretty)
    & vsep

type Input = Map (V2 Int) Cell

directions =
  [ V2   1   0
  , V2 (-1)  0
  , V2   0   1
  , V2   0 (-1)
  , V2   1   1
  , V2 (-1)  1
  , V2   1 (-1)
  , V2 (-1)(-1)
  ]

parser :: Parser Input
parser =
  chars
  <&> (\cs -> Map.fromList
              [ (V2 i j, x)
              | (i, xs) <- zip [0..] cs
              , (j, x) <- zip [0..] xs
              ]
      )
  where
    chars = (many cell) `sepBy` newline

    cell =
      asum
      [ "L" *> pure Empty
      , "." *> pure Floor
      , "#" *> pure Occupied
      ]

next1 :: Map (V2 Int) Cell -> Map (V2 Int) Cell
next1 map =
  map
  & Map.mapWithKey
  (\k v ->
     adjNeighbours k
     & filter ((==(Just Occupied)) . (map Map.!?))
     & length
     & step v
  )
  where
    step Empty n | n == 0 = Occupied
    step Occupied n | n >= 4 = Empty
    step i _ = i

    adjNeighbours p = fmap (^+^ p) directions


next2 :: Map (V2 Int) Cell -> Map (V2 Int) Cell
next2 map =
  map
  & Map.mapWithKey
  (\k v ->
     neighbours k
     & step v
  )
  where
    step Empty n | n == 0 = Occupied
    step Occupied n | n >= 5 = Empty
    step i _ = i

    neighbours :: V2 Int -> Int
    neighbours p =
      length
      [ ()
      | dir <- directions
      , (==(Just (Just Occupied)))
        . listToMaybe
        . dropWhile (==(Just Floor))
        . takeWhile isJust
        . fmap (map Map.!?)
        $ iterate (^+^ dir) (p ^+^ dir)
      ]


converge :: (a -> a -> Bool) -> [a] -> a
converge _ [] = error "Sequence doesn't `converge'"
converge cmp (x:y:rest)
  | cmp x y = x
  | otherwise = converge cmp (y:rest)

one :: Input -> _
one =
  converge (==) . fmap (length . filter ((==Occupied) . snd) . Map.toList) . iterate next1

two :: Input -> _
two =
  converge (==) . fmap (length . filter ((==Occupied) . snd) . Map.toList) . iterate next2

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day11.txt"

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
