{-

Problem 41. Pandigital prime

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?

-}

module Euler.P41 where

import Data.List
import Data.Char

import Debug.Trace

import Control.Applicative

import Euler.Util

isNPandigital :: Integer -> Bool
isNPandigital n =
    -- trace (show n) $
    -- length str < 10 &&
    sort str == [ '1' .. intToDigit (length str) ]
    where str = show n

-- given a base integer(in str)
genPan :: String -> Int -> [Integer]
genPan str max = do
    if length str >= max then
        fail "no further gen is possible"
    else do
        n <- filter ((`notElem` str) . intToDigit) [ 1 .. 9 ]

        let new = str ++ [ intToDigit n ]
            rnew = read new

            valid =
                if n `elem` [ 1, 3, 7, 9 ] &&
                   isPrime rnew && isNPandigital rnew then
                    return rnew
                else
                    fail "not pandigital"

        valid <|> genPan new max

-- 8 & 9 digit numbers are not possible
-- because the digit sum is divisble by 3
solve = last (genPan [] 7)
    -- filter isNPandigital $
    -- filter isPrime [ 1 .. 1000000 ]
