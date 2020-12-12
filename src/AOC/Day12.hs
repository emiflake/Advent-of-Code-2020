module AOC.Day12 where

import AOC.Common.Prelude

import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Linear
import Data.Semigroup
import Debug.Trace

data Instruction = Go Direction Int
  deriving (Show, Eq)

data Direction
  = North
  | South
  | East
  | West
  | TurnLeft
  | TurnRight
  | Forward
  deriving (Show, Eq)

type Input = [Instruction]

parser :: Parser Input
parser = many instruction
  where
    instruction =
      Go <$> direction <*> natural
    direction =
      asum
      [ "N" *> pure North
      , "S" *> pure South
      , "E" *> pure East
      , "W" *> pure West
      , "L" *> pure TurnLeft
      , "R" *> pure TurnRight
      , "F" *> pure Forward
      ]

one :: Input -> _
one =
  manhattan . absgo (^+^) const (V2 0 (0 :: Int)) (V2 1 0)

two :: Input -> _
two =
  manhattan . absgo const (^+^) (V2 0 (0 :: Int)) (V2 10 1)

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

absgo :: (V2 Int -> V2 Int -> V2 Int)
      -> (V2 Int -> V2 Int -> V2 Int)
      -> V2 Int
      -> V2 Int
      -> [Instruction]
      -> V2 Int
absgo _ _ position _ []                       = position
absgo f g position waypoint (Go dir amt:next) =
  case dir of
    North     -> absgo f g (f position (amt *^ (V2   0   1)))  (g waypoint (amt *^ (V2   0   1)))  next
    South     -> absgo f g (f position (amt *^ (V2   0 (-1)))) (g waypoint (amt *^ (V2   0 (-1)))) next
    East      -> absgo f g (f position (amt *^ (V2   1   0)))  (g waypoint (amt *^ (V2   1   0)))  next
    West      -> absgo f g (f position (amt *^ (V2 (-1)  0)))  (g waypoint (amt *^ (V2 (-1)  0)))  next
    Forward   -> absgo f g (position ^+^ (amt *^ waypoint))    waypoint                            next
    TurnLeft  -> absgo f g position                            (rotateLeft amt waypoint)           next
    TurnRight -> absgo f g position                            (rotateRight amt waypoint)          next

rotateLeft :: Int -> V2 Int -> V2 Int
rotateLeft amt v = iterate (*! V2 (V2 0 1)  (V2 (-1) 0)) v !! (amt `div` 90)

rotateRight :: Int -> V2 Int -> V2 Int
rotateRight amt v = iterate (*! V2 (V2 0 (-1))  (V2 1 0)) v !! (amt `div` 90)

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day12.txt"

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
