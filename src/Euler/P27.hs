{-

details see https://projecteuler.net/problem=27

-}

module Euler.P27 where

import Data.List

import Debug.Trace

import Euler.Util

poly :: Integer -> Integer -> Integer -> Integer
poly a b n = n ^ 2 + a * n + b

max' = maximumBy (\(l1, _, _) (l2, _, _) -> compare l1 l2)

-- -59231
solve = max' $ do
    b <- filter isPrime [ 0 .. 1000 ]

    return $ max' $ do
        a <- filter odd [ -b - 1 .. 1000 ]
        return (length (takeWhile (isPrime . poly a b) [0..]), a, b)

-- 1 + a + b > 0
-- a > -b - 1

-- n ^ 2 + a n + b
-- n(n + a) + b > 0
-- 2(2 + a) + b
-- 1 + -2 + b

-- when n = b, the result must be a composite
-- 
