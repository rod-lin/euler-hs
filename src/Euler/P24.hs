{-

Lexicographic permutations
Problem 24 
A permutation is an ordered arrangement of objects.
For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

-}

module Euler.P24 where

import Euler.Util

-- assuming the list is sorted
-- i counts from 1
nthPermOf :: (Ord t, Show t) => Int -> [t] -> [t]
nthPermOf 0 lst = lst
nthPermOf i lst =
    if i >= sub_perm * len then
        error ("not enought permutations(need " ++ show (i + 1) ++ ") for " ++ show lst)
    else
        -- see which sub permutation the target is in
        (lst !! sub_idx) : nthPermOf sub_nth sub_lst
    where len = length lst
          sub_perm = (len - 1) `permute` (len - 1) -- # of permutations for each sub list
          (sub_idx, sub_nth) = i `quotRem` sub_perm
          sub_lst = take sub_idx lst ++ drop (sub_idx + 1) lst

-- 2783915460
solve = nthPermOf (1000000 - 1) [ '0' .. '9' ]
