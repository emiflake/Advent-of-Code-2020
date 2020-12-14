{-# LANGUAGE RecordWildCards #-}
module Main where

import AOC.Common.Prelude hiding (Parser, option)
import AOC.Common.Config
import AOC.Day01 as Day01
import AOC.Day02 as Day02
import AOC.Day03 as Day03
import AOC.Day04 as Day04
import AOC.Day05 as Day05
import AOC.Day06 as Day06
import AOC.Day07 as Day07
import AOC.Day08 as Day08
import AOC.Day09 as Day09
import AOC.Day10 as Day10
import AOC.Day11 as Day11
import AOC.Day12 as Day12
import AOC.Day13 as Day13
import AOC.Day14 as Day14

import Data.Foldable
import Control.Lens ((^?), ix)

import Prettyprinter

import Options.Applicative


days :: [ SomeDay ]
days =
  [ SomeDay Day01.today
  , SomeDay Day02.today
  , SomeDay Day03.today
  , SomeDay Day04.today
  , SomeDay Day05.today
  , SomeDay Day06.today
  , SomeDay Day07.today
  , SomeDay Day08.today
  , SomeDay Day09.today
  , SomeDay Day10.today
  , SomeDay Day11.today
  , SomeDay Day12.today
  , SomeDay Day13.today
  , SomeDay Day14.today
  ]

run :: Int -> IO ()
run i =
  case days ^? ix (i - 1) of
    Nothing -> print ("Day" <+> pretty i <+> "not found" <> hardline)
    Just (SomeDay v) -> runDay Nothing Nothing (Flags True True True) i v

main :: IO ()
main = do
  Config{..} <- execParser opts
  case goal of
    All ->
      for_ (zip [1..] days) $ \(i, d) -> elimSomeDay (runDay Nothing Nothing flags i) d
    SpecificDay day fp command ->
      case days ^? ix (day - 1) of
        Nothing -> print ("Day" <+> pretty day <+> "not found" <> hardline)
        Just (SomeDay v) -> runDay command fp flags day v
