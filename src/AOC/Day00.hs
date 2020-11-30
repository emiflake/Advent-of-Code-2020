module AOC.Day00 where

import AOC.Common.Prelude

today :: Day [Int] Int Int
today =
  day
    (integer `sepBy` symbol ",")
    (pure . sum)
    (pure . product)
    []
    "inputs/day00.txt"
    [ ( "visual", putStrLn "im a pretty command that u just ran" )
    , ( "foo", putStrLn "im a different pretty command that u just ran" )
    ]

