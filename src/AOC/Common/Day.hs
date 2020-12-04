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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO as Text
import Control.Monad (when)
import Control.Lens ((^?), ix)
import System.Environment (withArgs)

import Prettyprinter
import Prettyprinter.Render.Terminal

data Day input a b =
  Day
  { _parseInput    :: Parser input
  , _solveOne      :: input -> IO a
  , _solveTwo      :: input -> IO b
  , _tests         :: SpecWith ()
  , _inputPath     :: FilePath
  , _extraCommands :: [(Text, IO ())]
  }

data SomeDay =
  forall input a b. (Show input, Show a, Show b) => SomeDay (Day input a b)

elimSomeDay ::
  (forall input a b. (Show input, Show a, Show b) => Day input a b -> c)
  -> SomeDay -> c
elimSomeDay f (SomeDay d) = f d

runTests :: Day input a b -> IO ()
runTests Day{..} =
  hspec _tests

parseInput :: Text -> Day input a b -> Either (ParseErrorBundle Text Void) input
parseInput sourceInput Day{..} =
  runMyParser _parseInput sourceInput

-- Horrible, I know.
runDay :: (Show input, Show a, Show b) => Maybe Text -> Maybe FilePath -> Flags -> Int -> Day input a b -> IO ()
runDay command maybeFile Flags{..} int day@Day{..} = do
  putDoc ("---" <+> annotate (color Blue) ("Day" <+> pretty int) <+> "---" <> hardline <> hardline)
  sourceInput <- Text.readFile (fromMaybe _inputPath maybeFile)
  case parseInput sourceInput day of
    Left e ->
      Prelude.putStrLn (errorBundlePretty e)
    Right v -> do
      putDoc (annotate (color Green) "Parsed input: " <> hardline)
      Prelude.putStrLn (take 80 (show v) <> "...")
      putDoc hardline
      when flagRunPartOne $ do
        putDoc (annotate (color Yellow) "Part one: " <> hardline)
        print =<< _solveOne v
        putDoc hardline
      when flagRunPartTwo $ do
        putDoc (annotate (color Yellow) "Part two: " <> hardline)
        print =<< _solveTwo v
        putDoc hardline

  withArgs [] $ runTests day

  case command of
    Just cmd ->
      case lookup cmd _extraCommands of
        Just io -> do
          putDoc (annotate (color Red) "Running command"
                  <+> annotate (color Yellow) (pretty cmd)
                  <> annotate (color Red) ":"
                  <> hardline <> hardline)
          io
        Nothing -> do
          putDoc (annotate (color Red) "Command"
                  <+> annotate (color Yellow) (pretty cmd)
                  <+> annotate (color Red) "not found"
                  <> hardline)
          when (length _extraCommands > 0) $
            putDoc ("Pick from"
                  <+> (align
                      . sep
                      . zipWith (<>) (mempty : repeat ", ")
                      . fmap (annotate (color Yellow)
                              . pretty
                              . fst) $ _extraCommands)
                  <> hardline)
    Nothing ->
      pure ()


day :: forall input a b. (Show input, Show a, Show b)
    => Parser input
    -> (input -> IO a)
    -> (input -> IO b)
    -> SpecWith ()
    -> FilePath
    -> [(Text, IO ())]
    -> Day input a b
day = Day

pureDay :: forall input a b. (Show input, Show a, Show b)
    => Parser input
    -> (input -> a)
    -> (input -> b)
    -> FilePath
    -> Day input a b
pureDay parse one two fp = Day parse (pure . one) (pure . two) (pure ()) fp []
