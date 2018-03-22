{-

Problem 43. Sub-string divisibility

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.

-}

module Euler.P43 where

import Data.Char
import Data.List

import Euler.Util

nextDigit :: [Char] -> [Char] -> Int -> [Char]
nextDigit exclude [ a, b ] d = do
    i <- filter (`notElem` exclude) [ '0' .. '9' ]

    if d `divides` read [ a, b, i ] then
        return i
    else
        fail "not valid character"

genNum :: [Integer]
genNum = do
    -- an init digit
    d1 <- [ '1' .. '9' ]
    d2 <- delete d1 [ '0' .. '9' ]
    d3 <- delete d2 $ delete d1 [ '0' .. '9' ]

    d4 <- nextDigit [ d1, d2, d3 ] [ d2, d3 ] 2
    d5 <- nextDigit [ d1, d2, d3, d4 ] [ d3, d4 ] 3
    d6 <- nextDigit [ d1, d2, d3, d4, d5 ] [ d4, d5 ] 5
    d7 <- nextDigit [ d1, d2, d3, d4, d5, d6 ] [ d5, d6 ] 7
    d8 <- nextDigit [ d1, d2, d3, d4, d5, d6, d7 ] [ d6, d7 ] 11
    d9 <- nextDigit [ d1, d2, d3, d4, d5, d6, d7, d8 ] [ d7, d8 ] 13
    dt <- nextDigit [ d1, d2, d3, d4, d5, d6, d7, d8, d9 ] [ d8, d9 ] 17

    return (read [ d1, d2, d3, d4, d5, d6, d7, d8, d9, dt ])

-- 16695334890
solve = sum genNum
