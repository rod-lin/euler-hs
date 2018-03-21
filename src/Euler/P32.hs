{-

Problem 32. Pandigital products

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

-}

module Euler.P32 where

import Data.List

import Debug.Trace

import Euler.Util

-- the search range would be 1 - 10000
-- no proof at the time, but number greater than 10000
-- has too many digits to satify the condition

isPandigital :: Integer -> Integer -> Integer -> Bool
isPandigital a b c =
    sort str == "123456789"
    where str = show a ++ show b ++ show c

-- O(n ^ 1.5)
-- 45228
solve = sum $ unique $ do
    n <- [ 1 .. 10000 ]
    a <- filter (`divides` n) [ 1 .. isqrt n + 1 ]
    let b = n `div` a

    -- traceM (show n)

    if isPandigital a b n then return n
    else fail "not pandigital"
