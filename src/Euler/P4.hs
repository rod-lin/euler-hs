{-

Problem 4. Largest palindrome product

A palindromic number reads the same both ways.
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.

-}

module Euler.P4 where

isPalindrome :: Integer -> Bool
isPalindrome n =
    reverse str == str
    where str = show n

threeDigitNums = [ 999, 998 .. 100 ]

-- 906609
solve =
    foldl max 0 ps
    where ps = filter isPalindrome [ a * b | a <- threeDigitNums, b <- threeDigitNums ]
