{-

Problem 3. Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143?

-}

module Euler.P3 where

import Euler.Util

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor 2 = 2
largestPrimeFactor n =
    if isPrime n then n
    else
        let factor = head (dropWhile (`divides` n) primes) in
        max factor (largestPrimeFactor (n `div` factor))

-- 6857
solve = largestPrimeFactor 600851475143
