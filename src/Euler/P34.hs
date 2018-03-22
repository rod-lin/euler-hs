{-

Problem 34. Digit factorials

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

-}

module Euler.P34 where

import Data.Char

import Euler.Util

-- search 1000000

-- 0 <= n < 10
ffact :: Integral t => t -> t
ffact = ([ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880 ] !!) . fi

isCurious :: Integer -> Bool
isCurious i =
    sum (map (fi . ffact . digitToInt) (show i)) == i

-- 40730
solve =
    sum (filter isCurious [ 3 .. 1000000 ])
