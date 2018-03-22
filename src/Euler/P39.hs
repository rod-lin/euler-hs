{-

Problem 39. Integer right triangles

If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

-}

module Euler.P39 where

import Data.List

import Euler.Util

import Debug.Trace

numOfSol :: Integer -> Int
numOfSol n = length $ trace (show n) $ do
    a <- [ 1 .. (n `div` 2) - 1 ]
    b <- [ max ((n `div` 2) - a + 1) 1 .. n `div` 2 - 1 ]
    let c = n - a - b

    if a ^ 2 + b ^ 2 == c ^ 2 then
        [(a, b, c)]
    else
        fail "not triangonal numbers"

solve =
    snd $
    maximumBy (\(a, _) (b, _) -> compare a b) $
    map (\n -> (numOfSol n, n)) [ 1 .. 1000 ]
