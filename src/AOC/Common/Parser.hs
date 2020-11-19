{-# LANGUAGE RecordWildCards #-}
module AOC.Common.Parser
  ( Parser
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , runMyParser
  )
where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text
--                   ^    ^
--                   |    \ Token stream
--                   |
--                   \ Custom error type

runMyParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runMyParser parser =
  parse parser ""
