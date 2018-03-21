module Euler.Util where

import Data.Bits
import Data.Char

import System.Random

import Debug.Trace

divides :: Integral t => t -> t -> Bool
divides a b = mod b a == 0

-- by trevordixon at https://gist.github.com/trevordixon/6788535
modExp :: (Integral t, Bits t) => t -> t -> t -> t
modExp b 0 m = 1
modExp b e m =
    t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

rand :: Int -> Int
rand = fst . next . mkStdGen

randList :: Int -> Int -> [Int]
randList seed n =
    map fst (take n (tail (iterate (next . snd) (0, gen))))
    where gen = mkStdGen seed

-- return if the number is a composite(except 2)
millerRabinTest :: (Integral t, Bits t, Show t) => t -> t -> Bool
millerRabinTest n a
    | a <= 1 || a >= n - 1 = error ("witness out of range a = " ++ show a)
    | n <= 3 = error "illegal range of n"
    | even n = True
    | otherwise = test1 && test2
    where
        -- let n = 2 ^ s * d
        find2sd s d =
            case mod d 2 of
                1 -> (s, d)
                _ -> find2sd (s + 1) (d `shiftR` 1)

        (s, d) = find2sd 0 (n - 1)

        test1 = (a `modExp` d) n /= 1
        test2 = all (\r -> (a `modExp` (2 ^ r * d)) n /= n - 1) [ 0 .. s - 1 ]

-- k = 5 in default
isPrime :: (Integral t, Bits t, Show t) => t -> Bool
isPrime n
    | n < 2 = False
    | n == 2 || n == 3 = True
    | otherwise = all (not . millerRabinTest n) as

    where as = map ((max 2) . (`mod` (n - 1)) . fromIntegral)
                   (randList (fromIntegral n) 10)

primes :: (Integral t, Bits t, Show t) => [t]
primes = 2 : filter isPrime [3..]

odds :: Integral t => [t]
odds = [ 3, 5 .. ]

evens :: Integral t => [t]
evens = [ 0, 2 .. ]

first :: (a -> Bool) -> [a] -> a
first pred =
    head . dropWhile (not . pred)

-- gcd of two numbers
egcd :: Integral t => t -> t -> t
egcd a 0 = a
egcd a b = egcd b (a `mod` b)

-- gcd of n numbers
egcds :: Integral t => [t] -> t
egcds [] = error "gcd of empty list"
egcds ns = foldl egcd (head ns) (tail ns)

elcm :: Integral t => t -> t -> t
elcm a b =
    gcd * n1 * n2
    where gcd = egcd a b
          n1 = a `div` gcd
          n2 = b `div` gcd

elcms :: Integral t => [t] -> t
elcms [] = error "lcm of empty list"
elcms [a] = a
elcms (a:rs) = elcm a (elcms rs)

sepWhen :: (a -> Bool) -> [a] -> ([a], [a])
sepWhen pred =
    foldl (\(l1, l2) v ->
        if pred v then
            (l1 ++ [v], l2)
        else
            (l1, l2 ++ [v])) ([], [])

for = flip map

vec2Add :: Num t => (t, t) -> (t, t) -> (t, t)
vec2Add (a, b) (c, d) = (a + c, b + d)

maybeCat :: [Maybe a] -> [a]
maybeCat lst = [ a | Just a <- lst ]

isqrt :: Integral t => t -> t
isqrt t = floor (sqrt (fi t))

divisors :: Integral t => t -> [t]
divisors n =
    part ++ reverse (map (n `div`) (if last part ^ 2 == n then init part else part))
    where part = filter (`divides` n) [ 1 .. isqrt n ]

properDivisors :: Integral t => t -> [t]
properDivisors = init . divisors

-- n >= 1
properDivisorSum :: Integral t => t -> t
properDivisorSum n =
    foldl (\s d -> let (v, m) = n `quotRem` d in
        if m == 0 then
            s + d + (if v == d then 0 else v)
        else s) 1 [ 2 .. isqrt n ]

choose :: Integral t => t -> t -> t
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n - 1) (k - 1) * n `div` k 

permute :: Integral t => t -> t -> t
permute n 0 = 1
permute 0 k = 0
permute n k = n * permute (n - 1) (k - 1)

fact :: Integral t => t -> t
fact n
    | n < 0 = error "fact of negative value"
    | otherwise = f n 1
    where
        f 0 a = a
        f 1 a = a
        f n a = f (n - 1) (a * n)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

digitSum :: Integral t => Integer -> t
digitSum n = fi (sum (map digitToInt (show n)))

fibs :: Integral t => [t]
fibs =
    map fst $ flip iterate (1, 1) $ \(a, b) ->
        (b, a + b)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
    | x `elem` xs  = unique xs
    | otherwise = x : unique xs
