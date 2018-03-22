{-

Problem 46. Goldbach's other conjecture

It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

-}

module Euler.P46 where

import Euler.Util

-- odd composite
-- that cannot be written as a prime and twice a square
valid :: Int -> Bool
valid n = null $ do
    p <- filter isPrime [ n - 2, n - 3 .. 2 ]

    let a = (n - p) `div` 2

    if isSquare a then return p
    else fail "no"

solve = first valid $ filter (\n -> odd n && not (isPrime n)) [2..]
