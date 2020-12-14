{-# LANGUAGE TupleSections #-}
module AOC.Day14 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import Data.Foldable
import Data.Bits
import Debug.Trace

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


data MaskElement = Undefined | Zero | One
  deriving (Show, Eq)

type Mask = [(Int, MaskElement)]

data Instruction
  = Mask Mask
  | Assign Int Integer
  deriving (Show, Eq)


type Input = [Instruction]

parser :: Parser Input
parser = many instruction
  where
    maskStream :: Parser [(Int, MaskElement)]
    maskStream =
        fmap (\(i,v) -> (35-i, v))
      . zip [0..]
      <$> some (asum [ symbol "0" *> pure Zero
                     , symbol "1" *> pure One
                     , symbol "X" *> pure Undefined
                     ])

    instruction =
      asum
      [ Mask
        <$ lexeme "mask"
        <* symbol "=" <*> maskStream
      , Assign
        <$ lexeme "mem" <* symbol "[" <*> natural <* symbol "]"
        <* symbol "=" <*> natural
      ]

type Memory = IntMap Integer

runMemoryTape :: [Instruction] -> Mask -> Memory -> Memory
runMemoryTape [] _ mem = mem
runMemoryTape (x:xs) mask mem =
  case x of
    Mask m ->
      runMemoryTape xs m mem
    Assign ptr value ->
      runMemoryTape xs mask (IntMap.insert ptr (maskOut mask value) mem)
  where
    maskOut maskElems start =
      foldr (\(i,t) a ->
               case t of
                 Undefined -> a
                 One -> a `setBit` i
                 Zero -> a `clearBit` i
            ) start maskElems

one :: Input -> _
one xs = sum . fmap snd . IntMap.toList $ runMemoryTape xs [] IntMap.empty

runMemoryTapeSuper :: [Instruction] -> Mask -> Memory -> Memory
runMemoryTapeSuper [] _ mem = mem
runMemoryTapeSuper (x:xs) mask mem =
  case x of
    Mask m ->
      runMemoryTapeSuper xs m mem
    Assign ptr value ->
      runMemoryTapeSuper xs mask
        (foldr (`IntMap.insert` value) mem (maskPtr mask ptr))

maskPtr :: [(Int, MaskElement)] -> Int -> [Int]
maskPtr [] ptr = [ptr]
maskPtr ((k,v):xs) ptr =
  case v of
    Undefined -> maskPtr xs ptr >>= \s -> [s `setBit` k, s `clearBit` k]
    One ->       fmap (`setBit` k) (maskPtr xs ptr)
    Zero ->      maskPtr xs ptr

two :: Input -> _
two xs = sum . fmap snd . IntMap.toList $ runMemoryTapeSuper xs [] IntMap.empty

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day14.txt"

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
