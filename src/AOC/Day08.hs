module AOC.Day08 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Control.Monad

import qualified Data.Set as Set
import Data.Set (Set)

type Input = [Instruction]

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show, Eq)

parser :: Parser Input
parser = many inst
  where 
        inst =
          asum
          [ Acc <$ lexeme "acc" <*> integer
          , Jmp <$ lexeme "jmp" <*> integer
          , Nop <$ lexeme "nop" <*> integer
          ]

runProgram :: Map Int Instruction -> Int
runProgram tape = go 0 0 Set.empty
  where
    go acc loc seen
      | loc `Set.member` seen = acc
      | otherwise =
          case tape ! loc of
            Acc offs ->
              go (acc + offs) (loc + 1) (Set.insert loc seen)
            Jmp offs ->
              go acc (loc + offs) (Set.insert loc seen)
            Nop _ ->
              go acc (loc + 1) (Set.insert loc seen)

runProgramChecked :: Map Int Instruction -> Maybe Int
runProgramChecked tape = last <$> go 0 0 Set.empty
  where
    go acc loc seen
      | loc `Set.member` seen = Nothing
      | otherwise =
          liftM2 (:) (pure acc) $
            case Map.lookup loc tape of
                  Just (Acc offs) ->
                    go (acc + offs) (loc + 1) (Set.insert loc seen)
                  Just (Jmp offs) ->
                    go acc (loc + offs) (Set.insert loc seen)
                  Just (Nop _) ->
                    go acc (loc + 1) (Set.insert loc seen)
                  Nothing -> Just []


invertInstruction :: Instruction -> Instruction
invertInstruction inst =
  case inst of
    Acc o -> Acc o
    Jmp o -> Nop o
    Nop o -> Jmp o

morphedPrograms :: Map Int Instruction -> [Map Int Instruction]
morphedPrograms insts =
  [ Map.adjust invertInstruction k insts
  | k <- [0..Map.size insts]
  ]


one :: Input -> _
one xs = runProgram . Map.fromList $ zip [0..] xs

two :: Input -> _
two xs = head . mapMaybe runProgramChecked . morphedPrograms . Map.fromList $ zip [0..] xs

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day08.txt"

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
