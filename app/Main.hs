{-# LANGUAGE RecordWildCards #-}
module Main where

import AOC.Common.Prelude hiding (Parser, option)
import AOC.Common.Config
import AOC.Day01 as Day01
import AOC.Day02 as Day02
import AOC.Day03 as Day03

import qualified Data.Text as Text
import Data.Text (Text)
import Text.Printf (printf)

import Data.Foldable
import Control.Lens ((^?), ix)

import Prettyprinter

import Options.Applicative
import Data.Semigroup ((<>))


days :: [ SomeDay ]
days =
  [ SomeDay Day01.today
  , SomeDay Day02.today
  , SomeDay Day03.today
  ]

run :: Int -> IO ()
run i =
  case days ^? ix (i - 1) of
    Nothing -> print ("Day" <+> pretty i <+> "not found" <> hardline)
    Just (SomeDay v) -> runDay Nothing Nothing (Flags True True True) i v

main :: IO ()
main = do
  cfg@Config{..} <- execParser opts
  case goal of
    All ->
      for_ (zip [1..] days) $ \(i, d) -> elimSomeDay (runDay Nothing Nothing flags i) d
    SpecificDay day fp command ->
      case days ^? ix (day - 1) of
        Nothing -> print ("Day" <+> pretty day <+> "not found" <> hardline)
        Just (SomeDay v) -> runDay command fp flags day v
