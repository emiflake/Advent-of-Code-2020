{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module AOC.Common.Day
where

import Data.Maybe (fromMaybe)
import AOC.Common.Config
import AOC.Common.Parser
import Test.Hspec
import Data.Void
import qualified Data.Text (Text)
import Data.Text as Text
import Data.Text.IO as Text
import Control.Monad (when)

import Prettyprinter
import Prettyprinter.Render.Terminal

data Day input a b =
  Day
  { _parseInput :: Parser input
  , _solveOne   :: input -> IO a
  , _solveTwo   :: input -> IO b
  , _tests      :: [SpecWith ()]
  , _inputPath  :: FilePath
  }

data SomeDay =
  forall input a b. (Show input, Show a, Show b) => SomeDay (Day input a b)

elimSomeDay ::
  (forall input a b. (Show input, Show a, Show b) => Day input a b -> c)
  -> SomeDay -> c
elimSomeDay f (SomeDay d) = f d

runTests :: Day input a b -> IO ()
runTests Day{..} =
  hspec $ sequence_ _tests

parseInput :: Text -> Day input a b -> Either (ParseErrorBundle Text Void) input
parseInput sourceInput Day{..} =
  runMyParser _parseInput sourceInput

runDay :: (Show input, Show a, Show b) => Maybe FilePath -> Flags -> Int -> Day input a b -> IO ()
runDay maybeFile Flags{..} int day@Day{..} = do
  putDoc ("---" <+> annotate (color Blue) ("Day" <+> pretty int) <+> "---" <> hardline <> hardline)
  sourceInput <- Text.readFile (fromMaybe _inputPath maybeFile)
  case parseInput sourceInput day of
    Left e ->
      print e
    Right v -> do
      putDoc (annotate (color Green) "Parsed input: " <> hardline)
      print v
      putDoc hardline
      when flagRunPartOne $ do
        putDoc (annotate (color Yellow) "Part one: " <> hardline)
        print =<< _solveOne v
        putDoc hardline
      when flagRunPartTwo $ do
        putDoc (annotate (color Yellow) "Part two: " <> hardline)
        print =<< _solveTwo v
        putDoc hardline
  when flagRunTests $
    case _tests of
      [] -> putDoc (annotate (color Black) "No tests to run" <> hardline)
      _ -> runTests day

day :: forall input a b. (Show input, Show a, Show b)
    => Parser input
    -> (input -> IO a)
    -> (input -> IO b)
    -> [SpecWith ()]
    -> FilePath
    -> Day input a b
day = Day

pureDay :: forall input a b. (Show input, Show a, Show b)
    => Parser input
    -> (input -> a)
    -> (input -> b)
    -> FilePath
    -> Day input a b
pureDay parse one two fp = Day parse (pure . one) (pure . two) [] fp
