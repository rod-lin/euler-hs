{-

Problem 37. Truncatable primes

The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-}

module Euler.P37 where

import Debug.Trace

import Euler.Util

-- valid in the other direction
rightValid :: Integer -> Bool
rightValid n =
    n >= 10 && -- not 1 digit number
    all (\n -> isPrime (read (drop n str) :: Integer)) range
    where str = show n
          range = [ 1 .. length str - 1 ]

tp :: Int -> [Integer]
tp 1 = filter isPrime [ 2 .. 9 ]
tp len = sub ++ do
    n <- sub
    d <- [ 1 .. 9 ]

    let new = n * 10 + d

    if isPrime new && new `notElem` sub then
        return new
    else
        fail "no valid"

    where sub = tp (len - 1)

-- [23,37,53,73,313,317,373,797,3137,3797,739397]
-- 748317
solve =
    sum (take 11 (filter rightValid (tp 1024)))
