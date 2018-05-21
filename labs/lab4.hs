-- CSci 60 Lab 4

---- Solution to Extended GCD (again) ----

-- usage: extGCD a b = (d, m, n)
-- assumes: a >= 0, b >= 0
-- insures: d = gcd(a,b), m*a + n*b = d
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b = let (q, r) = divMod a b
                 (d, m, n) = extGCD b r
             in (d, n, m - n*q)


---- Part I:  Some helper functions ----

-- Coprime with a list
-- usage: coprime_with x ys = ans
-- assumes: nothing
-- insures: ans == True, if x is coprime with each element of ys
--          ans == False, otherwise
-- every y in ys is coprime to x
coprime_with :: Integer -> [Integer] -> Bool
coprime_with x [] = True
coprime_with x (y:ys) = gcd x y == 1 && coprime_with x ys



-- Pairwise Coprime
-- usage: pairwise_coprime xs = ans
-- assumes: nothing
-- insures: ans == True, if the elements of the list xs are pairwise coprime
--          ans == False, otherwise
pairwise_coprime :: [Integer] -> Bool
pairwise_coprime [] = True
pairwise_coprime (x:xs) = coprime_with x xs && pairwise_coprime xs


-- Multiplicative inverse (using extGCD)
-- usage: minv a m = ainv
-- assumes: a >= 0, m >= 2, and gcd(a,m) = 1
-- insures: 0 <= ainv < m, a * ainv ≡ 1 (mod m)
minv :: Integer -> Integer -> Integer
minv a m = let (1,b,n) = extGCD a m
           in b `mod` m


---- Part II: Chinese Remainder Theorem and Exponential Mod ----

-- Does a given Chinese Remainder Theorem problem have a solution?
-- usage: crtHasSolution as ms = ans
-- insures: ans == True, if
--                  (1) the lists as and ms are the same length and nonempty,
--                  (2) each element mi of ms satisfies mi >= 2, and
--                  (3) the elements of ms are pairwise coprime;
--          ans == False, otherwise.
crtHasSolution :: [Integer] -> [Integer] -> Bool
crtHasSolution as ms = length as == length ms && all (>=2) ms && pairwise_coprime ms && all (>=0)as


-- Is a given number a solution to a CRT problem?
-- usage: crtIsSolution n as ms = ans
-- insures: ans == True, if crtHasSolution as ms == True and n is a solution
--          ans == False, otherwise
crtIsSolution :: Integer -> [Integer] -> [Integer] -> Bool
crtIsSolution n as ms = crtHasSolution as ms && map (mod n) as == ms


-- Chinese Remaninder Theorem
-- usage: crt as ms = ans
-- assumes: nothing
-- insures: ans == Nothing, if crtHasSolution as ms == False
--          ans == Just n, if n is such that crtIsSolution n as ms == True
--                         and 0 <= n < product ms                 
crt :: [Integer] -> [Integer] -> Integer
crt as ms = let total = product ms
                comp ai mi = let bigm = div total mi
                             in ai * bigm *(minv bigm mi)
            in  sum (zipWith comp as ms) `mod` total


-- Implement (efficient) modular exponentiation
-- usage: expmod a n m = b
-- assumes: n >= 0, m >= 2
-- insures: a^n ≡ b (mod m), 0 <= b < m
expmod :: Integer -> Integer -> Integer -> Integer
expmod a n m | n == 0 = 1
             | n == 1 = a `mod` m
             | even n = let q  = (expmod a (n `div` 2) m) `mod` m in (q^2) `mod` m
             | otherwise = (a * expmod a (n-1) m) `mod` m
-- quotient, n div 2
-- base case: when n==0 =1, case n ==1 = a `mod` m
-- expmod 4 53 14 = 2 
-- example: expmod 5 39 11 = 9

