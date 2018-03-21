{-

Problem 19. Counting Sundays

You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.

    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.

    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-}

module Euler.P19 where

import Euler.Util

-- count in 'days from 1 Jan 1900'(DF1J1900)

type DF1J1900 = Int
type Year = Int
type Month = Int -- count from 1

-- 1   2      3   4   5   6   7   8   9   10  11  12
-- 31  28/29  31  30  31  30  31  30  31  30  31  30  365/366

isSunday :: DF1J1900 -> Bool
isSunday d = d `mod` 7 == 6

isLeapYear :: Year -> Bool
isLeapYear y =
    if 100 `divides` y then 400 `divides` y
    else 4 `divides` y

monthLength :: Year -> Month -> DF1J1900
monthLength y m =
    if m /= 2 then
        [ undefined, 31, undefined, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ] !! m
    else if isLeapYear y then 29
    else 28

table :: [(DF1J1900, Month, Year)] -- (beginning of the month, month number, year number)
table = flip iterate (0, 1, 1900) $ \(d, m, y) ->
    ((monthLength y m) + d, if m == 12 then 1 else m + 1, if m == 12 then y + 1 else y)

isMonthYear :: Month -> Year -> (DF1J1900, Month, Year) -> Bool
isMonthYear m' y' (_, m, y) = m == m' && y == y'

solve =
    length $
    filter (\(d, _, _) -> isSunday d) $
    takeWhile (not . isMonthYear 1 2001) $
    dropWhile (not . isMonthYear 1 1901) table
