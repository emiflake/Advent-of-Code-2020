module AOC.Day04 where

import AOC.Common.Prelude

import Data.Maybe
import qualified Data.Text as Text

import Data.Foldable
import Control.Monad

type Input = [[(String,String)]]

parser :: Parser Input
parser = passport `sepBy` newline
  where
    passport =
      many (field <* (char ' ' <|> char '\n'))

    field =
      (,) <$> many letterChar <* symbol ":" <*> many (alphaNumChar <|> char '#')

requiredFields :: [String]
requiredFields = [ "byr" , "iyr" , "eyr" , "hgt" , "hcl" , "ecl" , "pid" ]

isValidField :: String -> String -> Bool
isValidField field value =
  let
    parseField =
      case field of
        "byr" -> (\x -> x >= 1920 && x <= 2002) <$> integer
        "iyr" -> (\x -> x >= 2010 && x <= 2020) <$> integer
        "eyr" -> (\x -> x >= 2020 && x <= 2030) <$> integer
        "hgt" -> (\x unit -> (unit == "cm" && 150 <= x && x <= 193)
                          || (unit == "in" && 59  <= x && x <= 76))
                 <$> integer <*> (lexeme "cm" <|> lexeme "in")
        "hcl" -> const True <$> (char '#' *> replicateM 6 hexDigitChar)
        "ecl" -> const True <$> asum [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
        "pid" -> const True <$> replicateM 9 digitChar
        _ -> pure True
  in
  case runMyParser (parseField <* eof) (Text.pack value) of
    Left _ -> False
    Right v -> v

isValid :: [(String, String)] -> Bool
isValid passport =
  all (\field -> isJust $ field `lookup` passport) requiredFields

isValidCheckFields :: [(String, String)] -> Bool
isValidCheckFields passport =
  all (\field -> maybe False (isValidField field) $ lookup field passport) requiredFields

one :: Input -> Int
one xs =
  length . filter isValid $ xs

two :: Input -> Int
two xs =
  length . filter isValidCheckFields $ xs

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day04.txt"

commands :: [(Text, IO ())]
commands =
  []

today :: Day Input Int Int
today =
  day
    parser
    (pure . one)
    (pure . two)
    tests
    inputFile
    commands
