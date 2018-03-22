{-

Champernowne's constant
Problem 40 
An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

-}

module Euler.P40 where

import Data.Char

import Debug.Trace

-- limits :: [Integer]
limits =
    map snd $ iterate (\(n, a) -> (n + 1, 9 * (10 ^ (n - 1)) * n + a)) (1, 0)

d :: Int -> Int
d n =
    trace (show (num, num_len, last lim, rest)) $ digitToInt (show num !! rest)
    where lim = takeWhile (n >) limits
          num_len = length lim
          (num', rest) = (n - last lim - 1) `quotRem` num_len -- the number the index falls at
          num = num' + 10 ^ (num_len - 1)

-- n (n - 9) * 2 + 9

-- < 10: 9
-- < 100: 90 * 2 + 9
-- < 1000: 900 * 3 + 90 * 2 + 9
-- < 10000: 9000 * 4 + 900 * 2 + 9

solve = product (map d [ 1, 10, 100, 1000, 10000, 100000, 1000000 ])
