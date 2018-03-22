{-

Problem 49. Prime permutations

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?

-}

module Euler.P49 where

import Data.List

import Euler.Util

valid :: Integer -> Bool
valid n =
    all isPrime l &&
    allEq (map (sort . show) l)
    where a = n
          b = n + 3330
          c = b + 3330
          l = [ a, b, c ]

-- ["148748178147","296962999629"]
solve =
    map (\a -> show a ++ show (a + 3330) ++ show (a + 6660)) $
    filter valid [ 1000 .. 9999 ]
