{-

Problem 47. Distinct primes factors

The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?

-}

module Euler.P47 where

import Debug.Trace

import Euler.Util

-- n, n + 1, n + 2, n + 3
-- each have 4 distinct prime factors
valid :: Int -> Int -> Bool
valid a n =
    let check = (>= a) . length . unique . primeFactor
    in and (map (check . (n +)) [ 0 .. a - 1 ])

-- 134043
solve = first (valid 4) [1..]
