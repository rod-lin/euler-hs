{-

Problem 31. Coin sums

In England the currency is made up of pound, £, and pence, p,
and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?

-}

module Euler.P31 where

import Euler.Util

import Debug.Trace

coins = [ 1, 2, 5, 10, 20, 50, 100, 200 ]

search :: Integer -> [Integer] -> Integer
search 0 _ = 1
search _ [] = 0
search value (c:cs) = sum $ do
    n <- [ 0 .. value `div` c ]
    return (search (value - n * c) cs)
    
-- 73682
solve = search 200 (reverse coins)
-- 200 * 100 * 40 * 20 * 10 * 4 * 2 * 1 routes
