{-

Problem 33. Digit cancelling fractions

The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

-}

module Euler.P33 where

import Euler.Util

-- 10 <= a < b < 100
-- a / b == n2 / d2
-- a * d2 / b == n2
valid :: Word -> Word -> Bool
valid a b
    | n1 == d1 = fi a / fi b == fi n2 / fi d2
    | n1 == d2 = fi a / fi b == fi n2 / fi d1
    | n2 == d1 = fi a / fi b == fi n1 / fi d2
    | n2 == d2 = fi a / fi b == fi n1 / fi d1
    | otherwise = False
    where (n1, n2) = a `quotRem` 10
          (d1, d2) = b `quotRem` 10

trivial :: Word -> Word -> Bool
trivial a b = a `mod` 10 == 0 && b `mod` 10 == 0

-- 100
solve =
    let (n, d) = foldl (\(a, b) (c, d) -> (a * c, b * d)) (1, 1) $ do
            a <- [ 10 .. 99 ]     
            b <- [ 10 .. 99 ]

            if a < b && valid a b && not (trivial a b) then
                return (a, b)
            else
                fail "not valid"
    in d `div` egcd n d
