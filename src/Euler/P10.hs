{-

Problem 10. Summation of primes

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

-}

module Euler.P10 where

import Euler.Util

-- 142913828922
-- TODO: O P T I M I Z E T H I S S T U P I D C O D E
solve = -- sum (takeWhile (<= 2000000) primes) :: Integer
    sum (filter isPrime [ 1 .. 2000000 - 1 ]) :: Integer
