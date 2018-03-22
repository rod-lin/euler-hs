{-

Problem 45. Triangular, pentagonal, and hexagonal

Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

Triangle	 	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
Hexagonal	 	Hn=n(2n−1)	 	1, 6, 15, 28, 45, ...
It can be verified that T285 = P165 = H143 = 40755.

Find the next triangle number that is also pentagonal and hexagonal.

-}

module Euler.P45 where

import Euler.Util

-- 1533776805
solve =
    head $ head $
    commonHead $ map tail $
    commonHead $ map tail $
    [ [ n * (n + 1) `div` 2 | n <- [1..] ],
      [ n * (3 * n - 1) `div` 2 | n <- [1..] ],
      [ n * (2 * n - 1) | n <- [1..] ] ]