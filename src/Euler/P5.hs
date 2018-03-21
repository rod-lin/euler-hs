{-

Problem 5. Smallest multiple

2520 is the smallest number that can be divided by each of
the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-}

module Euler.P5 where

import Euler.Util

allDivisible nums v = and (map (`divides` v) nums)

-- 232792560
solve :: Integer
solve = -- first (allDivisible [ 2 .. 20 ]) [20..]
    elcms [ 1 .. 20 ]
