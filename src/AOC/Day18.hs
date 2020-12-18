module AOC.Day18 where

import AOC.Common.Prelude

import Data.List
import Data.Maybe
import Data.Foldable
import Text.Printf


data Op = Mul | Add
  deriving (Show, Eq)

data Expr
  = BinOp Op Expr Expr
  | Val Integer
  deriving (Show, Eq)

type Input = ([Expr], [Expr])

opLevelLeft :: Parser Op -> Parser Expr -> Parser Expr
opLevelLeft op descend =
  let
    helper :: Expr -> Parser Expr
    helper lhs =
      asum
      [ try $ do
          _ <- sc
          op' <- op
          _ <- sc
          rhs <- descend
          helper (BinOp op' lhs rhs)
      , pure lhs
      ]
  in
    helper =<< descend

parse1 :: Parser [Expr]
parse1 = many binop
  where
    term =
      asum
      [ Val <$> natural
      , symbol "(" *> binop <* symbol ")"
      ]

    binop =
      opLevelLeft op term

    op =
      asum
      [ symbol "*" *> pure Mul
      , symbol "+" *> pure Add
      ]

parse2 :: Parser [Expr]
parse2 = many mul
  where
    term =
      asum
      [ Val <$> natural
      , symbol "(" *> mul <* symbol ")"
      ]

    add =
      opLevelLeft (symbol "+" *> pure Add) term

    mul =
      opLevelLeft (symbol "*" *> pure Mul) add

parser :: Parser Input
parser = (,) <$> lookAhead parse1 <*> lookAhead parse2

eval :: Expr -> Integer
eval (Val x) = x
eval (BinOp op lhs rhs) =
  case op of
    Mul -> eval lhs * eval rhs
    Add -> eval lhs + eval rhs

one :: Input -> _
one = sum . fmap eval . fst

two :: Input -> _
two = sum . fmap eval . snd

tests :: SpecWith ()
tests = pure ()

inputFile :: FilePath
inputFile = "inputs/day18.txt"

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
