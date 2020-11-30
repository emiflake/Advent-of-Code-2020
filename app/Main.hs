{-# LANGUAGE RecordWildCards #-}
module Main where

import AOC.Common.Prelude hiding (Parser, option)
import AOC.Common.Config
import AOC.Day00 as Day00

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
  [ SomeDay Day00.today 
  , SomeDay Day00.today ]

main :: IO ()
main = do
  cfg@Config{..} <- execParser opts
  case goal of
    All ->
      for_ (zip [0..] days) $ \(i, d) -> elimSomeDay (runDay Nothing Nothing flags i) d
    SpecificDay day fp command ->
      case days ^? ix day of
        Nothing -> print ("Day" <+> pretty day <+> "not found" <> hardline)
        Just (SomeDay v) -> runDay command fp flags day v
