{-

Problem 38. Pandigital multiples

Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576
By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

-}

module Euler.P38 where

import Data.List

import Euler.Util

isPandigital :: String -> Bool
isPandigital msg = sort msg == [ '1' .. '9' ]

concatList :: Integer -> [String]
concatList n =
    take 9 $
    map head $
    iterate (\(a:b:l) -> (a ++ b) : l) $
    map (show . (n *)) [ 1 .. 9 ]

valid :: Integer -> Integer
valid n =
    case findIndex isPandigital l of
        Just i -> read (l !! i)
        Nothing -> 0

    where l = concatList n

-- search range 1 - 99999
-- 932718654
solve =
    case findIndex (/= 0) found of
        Just i -> found !! i
        Nothing -> error "not found"

    where range = [ 99999, 99998 .. 1 ]
          found = map valid range

