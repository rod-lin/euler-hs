{-

Problem 1. Multiples of 3 and 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.

-}

module Euler.P1 where

import Euler.Util

multipleOf :: [Int] -> Int -> Bool
multipleOf ns v = or (map (`divides` v) ns)

-- 233168
solve :: Int
solve =
    sum (filter (multipleOf [ 3, 5 ]) [ 1 .. 999 ])
