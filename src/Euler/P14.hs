{-

Problem 14. Longest Collatz sequence

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem),
it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.

-}

module Euler.P14 where

import qualified Data.Map.Strict as MP

import Debug.Trace

import Euler.Util

-- search + memorization

type MemTable = MP.Map Integer Integer

-- search' :: MemTable -> Integer -> Integer -> (Integer, MemTable)
-- search' mem0 n maxn =
--     -- either (n - 1) / 3
--     -- or n * 2

--     trace (show n) $
--     case MP.lookup n mem0 of
--         Just res -> (res, mem0)
--         Nothing ->
--             if n >= maxn then (0, mem0)
--             else (res, MP.insert n res mem2)

--     where
--         (b1, mem1) = search' mem0 (n * 2) maxn
--         (b2, mem2) =
--             if 3 `divides` (n - 1) && odd ((n - 1) `div` 3) then
--                 search' mem1 ((n - 1) `div` 3) maxn
--             else
--                 (0, mem1)
        
--         res = 1 + max b1 b2

-- search = search' (MP.empty)

search :: MemTable -> Integer -> (Integer, MemTable)
search mem n =
    if n == 1 then (1, mem)
    else case MP.lookup n mem of
        Just res -> (res, mem)
        Nothing -> (res + 1, MP.insert n (res + 1) mem1)

    where
        n' = if even n then n `div` 2 else n * 3 + 1
        (res, mem1) = search mem n'

-- 837799
solve =
    foldl (\(a, l1) (b, l2) -> if l2 > l1 then (b, l2) else (a, l1)) (0, 0) $
    MP.toList $
    foldl (\m i -> snd (search m i)) MP.empty
    [ 1 .. 1000000 - 1 ]
