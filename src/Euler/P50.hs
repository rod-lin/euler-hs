{-

Problem 50. Consecutive prime sum

The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?

-}

module Euler.P50 where

import Debug.Trace

import Data.List

import Euler.Util

limit = 1000000
ps = filter isPrime [ 1 .. limit ]
nps = 78498 -- == length ps

sums :: [(Int, Int)]
sums = do
    start <- [ 0 .. nps `div` 2 ]

    let res =
            filter (isPrime . fst) $
            takeWhile ((< limit) . fst) $ do
                len <- filter odd [ 1 .. nps - start ]

                let lst = take len (drop start ps)
                    n = sum lst

                return (n, length lst)

    if null res then fail "no match"
    else return (maximumBy (\(_, l1) (_, l2) -> compare l1 l2) res)

-- 997651 chain length 543
solve = maximumBy (\(_, l1) (_, l2) -> compare l1 l2) sums
    -- where
    --     primes = filter isPrime [ 1 .. 1000000 ]
