module AOC.Common.ParseTools where

import AOC.Common.Parser
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Text as Text
import Data.Text (Text)
  
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser Text
stringLiteral = char '\"' *> (Text.pack <$> manyTill L.charLiteral (char '\"'))

float :: Parser Double
float = try (lexeme L.float) <|> lexeme L.decimal

natural :: Parser Int
natural = lexeme L.decimal

integer :: Parser Int
integer = L.signed mempty natural
