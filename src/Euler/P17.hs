{-

Problem 17. Number letter counts

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

-}

module Euler.P17 where

-- without spaces or hyphens
numToWord :: (Show t, Integral t) => t -> String
numToWord 1 = "one"
numToWord 2 = "two"
numToWord 3 = "three"
numToWord 4 = "four"
numToWord 5 = "five"
numToWord 6 = "six"
numToWord 7 = "seven"
numToWord 8 = "eight"
numToWord 9 = "nine"
numToWord 10 = "ten"
numToWord 11 = "eleven"
numToWord 12 = "twelve"
numToWord 13 = "thirteen"
numToWord 14 = "fourteen"
numToWord 15 = "fifteen"
numToWord 16 = "sixteen"
numToWord 17 = "seventeen"
numToWord 18 = "eighteen"
numToWord 19 = "nineteen"

numToWord 20 = "twenty"
numToWord 30 = "thirty"
numToWord 40 = "forty"
numToWord 50 = "fifty"
numToWord 60 = "sixty"
numToWord 70 = "seventy"
numToWord 80 = "eighty"
numToWord 90 = "ninety"

numToWord 1000 = "onethousand"

numToWord n
    | n < 100 {- and n > 20 -} =
        numToWord (n - dec1) ++ numToWord dec1
    
    | dec2 == 0 = numToWord (n `div` 100) ++ "hundred"

    | n < 1000 {- 100 < n -} =
        numToWord (n - dec2) ++ if dec2 /= 0 then "and" ++ numToWord dec2 else ""

    | otherwise = error ("unsupported n " ++ show n)

    where dec1 = mod n 10
          dec2 = mod n 100

-- 21124
solve = -- sum (map (length . numToWord) [ 1 .. 150 ])
    foldl (\s n -> s + length (numToWord n)) 0 [ 1 .. 1000 ]
