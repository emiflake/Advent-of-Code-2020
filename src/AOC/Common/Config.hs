module AOC.Common.Config where

import Options.Applicative
import Data.Semigroup ((<>))

data Goal
  = All
  | SpecificDay Int (Maybe FilePath)
  deriving (Show, Eq)

data Config =
  Config
  { flags :: Flags
  , goal :: Goal
  }
  deriving (Show, Eq)

data Flags =
  Flags
  { flagRunTests :: Bool
  , flagRunPartOne :: Bool
  , flagRunPartTwo :: Bool
  }
  deriving (Show, Eq)

parseFlags :: Parser Flags
parseFlags =
  Flags
  <$> flag True False
      ( long "test"
     <> short 't'
     <> help "Run tests")
  <*> flag True False
      ( long "one"
     <> short '1'
     <> help "Run part one")
  <*> flag True False
      ( long "two"
     <> short '2'
     <> help "Run part two")

specificDay :: Parser Goal
specificDay =
  SpecificDay
  <$> argument auto (metavar "DAY")
  <*> optional (strOption
                ( long "file"
               <> short 'f'
               <> metavar "FILENAME"
               <> help "Input file" ))

parseGoal :: Parser Goal
parseGoal =
  subparser
    ( command "day" (info specificDay (progDesc "Run a specific day"))
   <> command "all" (info (pure All) (progDesc "Run all days")))

config :: Parser Config
config =
  Config
  <$> parseFlags
  <*> parseGoal

opts :: ParserInfo Config
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Advent of Code 2020"
  <> header "Advent-of-Code-2020"
  )
