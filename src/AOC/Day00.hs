module AOC.Day00 where

import AOC.Common.Prelude

today :: Day [Int] Int Int
today =
  pureDay
    (integer `sepBy` symbol ",")
    sum
    product
    "inputs/day00.txt"
