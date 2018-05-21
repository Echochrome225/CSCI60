-- Lab 6: Tracing and Boolean warm-up

import Debug.Trace (trace)


---- Functions from earlier labs ----------------------------------------------

-- Implement (efficient) modular exponentiation
-- usage: expmod a n m = b
-- assumes: n >= 0, m >= 2
-- insures: a^n ≡ b (mod m), 0 <= b < m
expmod :: Integer -> Integer -> Integer -> Integer
expmod a n m | trace (show (a,n,m)) False = undefined
expmod a 0 m = 1
expmod a n m = let (q,r) = divMod n 2
                   ans1 = expmod a q m
               in if r == 0 then ans1*ans1 `mod` m else ans1*ans1*a `mod` m
               --in trace ("---> " ++ show ans) ans

toBase :: Integer -> Integer -> [Integer]
toBase b n | trace (show (b,n)) False = undefined
toBase b n = let ans = reverse (toBase' b n) in trace ("---> " ++ show ans) ans where
  toBase' :: Integer -> Integer -> [Integer]
  toBase' b 0 = []
  toBase' b n = let (q,r) = divMod n b
                    ans = r : toBase' b q
                in trace ("---> " ++ show ans) ans
  --divide n by base add r to beginning

fromBase :: Integer -> [Integer] -> Integer
fromBase b xs | trace (show (b,xs)) False = undefined
fromBase b xs = let ans = fromBase' b (reverse xs) in trace ("---> " ++ show ans) ans where
  fromBase' :: Integer -> [Integer] -> Integer
  fromBase' b [] = let ans = 0 in trace ("---> " ++ show ans) ans
  fromBase' b (x:xs) = let ans = x + b * fromBase' b xs in trace ("---> " ++ show ans) ans
  
-- Tracing version of extGCD (from Piazza @81)
extGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extGCD a b | trace (show (a,b)) False = undefined
extGCD a 0 = let ans = (a,1,0) in trace ("---> " ++ show ans) ans
extGCD a b = let (q,r) = divMod a b
                 (d,m,n) = extGCD b r
                 ans = (d,n,m-q*n)
             in trace ("---> " ++ show ans) ans
  --fails first guard, prints out a and b

---- Part 1: Tracing ----------------------------------------------------------
{-
toBase 5 413
(5,413)
---> [3]
---> [1,3]
---> [2,1,3]
---> [3,2,1,3]
---> [3,1,2,3]
[3,1,2,3]

fromBase 5 [3,1,2,3]
(5,[3,1,2,3])
---> 0
---> 3
---> 16
---> 82
---> 413
---> 413
413
-}
-- Create tracing versions of expmod, toBase, and fromBase above
-- and cut and paste sample output here. Note that you will also need
-- to trace the helper functions toBase' and fromBase'.


---- Part 2: Boolean warm-up --------------------------------------------------

bools = [True, False]


-- We can get a list of all possible outputs of the not and && operations

not_vals = [not p | p <- bools]
and_vals = [p && q | p <- bools, q <- bools]

-- Do the same with the operators ||, <, <=, >, >=,  ==, and /=.

or_vals = [p || q | p <- bools, q <- bools]
less_vals = [p < q | p <- bools, q <- bools]
leq_vals = [p <= q | p <- bools, q <- bools]
greater_vals = [p > q | p <- bools, q <- bools]
geq_vals = [p >= q | p <- bools, q <- bools]
eq_vals = [p == q | p <- bools, q <- bools]
neq_vals = [p /= q | p <- bools, q <- bools]


-- The expressions p && q and q && p always give the same result, which we
-- can check in two different ways:

and1 = [p && q | p <- bools, q <- bools] == [q && p | p <- bools, q <- bools]
and2 = and [(p && q) == (q && p) | p <- bools, q <- bools]
