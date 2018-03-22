{-

Problem 48. Self powers

The series, 11 + 22 + 33 + ... + 1010 = 10405071317.

Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.

-}

module Euler.P48 where

import Euler.Util

solve :: Integer
solve =
    foldl (\a n -> (a + (n `modExp` n) m) `mod` m) 0 [ 1 .. 1000 ]
    where m = 10 ^ 10
