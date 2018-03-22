{-

Problem 36. Double-base palindromes

The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)

-}

module Euler.P36 where

import Euler.Util

valid :: Int -> Bool
valid n = isPalindrome (show n) && isPalindrome (bits n)

-- 872187
solve = sum (filter valid [ 0 .. 1000000 - 1 ])
