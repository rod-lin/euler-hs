{-

Problem 15. Lattice paths

Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

<picture see https://projecteuler.net/problem=15>

How many such routes are there through a 20×20 grid?

-}

module Euler.P15 where

import Euler.Util

-- naive method
route :: Int -> Int -> Int
route r c = (r + c) `choose` c

{-

basic idea

given a board of r * c

every route configuration consists of
(c + 1) integers indicating the number of down turn at each column
And the total number of down turns available is r(number of rows)

So we just need to put these r down turns to (c + 1) places while allowing
any place to have no down turns

So according to the stars & bars trick, we put n = r to k = c + 1 places

number of different ways
= (n + k - 1) `choose` n
= (r + c + 1 - 1) `choose` r
= (r + c) `choose` r

-}

-- 137846528820
solve = route 20 20
