-- CSci 60 Lab 7

bools = [True, False]

-- Similar to one of the answers from Lab 6, the following code exhaustively
-- checks whether not(P) \/ Q is equivalent to P -> Q.
not_equiv = and [(not p || q) == (p <= q) | p <- bools, q <- bools]


-- Write similar definitions that check each of the following equivalences:

-- P /\ Q = Q /\ P                           and is commutative
and_comm = and [(p && q) == (q && p) | p <- bools, q <- bools]
-- P \/ Q = Q \/ P                           or is commutative
or_comm = and [(p || q) == (q || p) | p <- bools, q <- bools]
-- P /\ (P -> Q) = P /\ Q
and_imp = and [(p && (p <= q)) == (p && q) | p <- bools, q <- bools]
-- P -> (P -> Q) = P -> Q
imp_imp = and [(p <= (p <= q)) == (p <= q) | p <- bools, q <- bools]

-- P /\ (Q /\ R) = (P /\ Q) /\ R             and is associative
and_assoc = and [(p && (q && r)) == ((p && q) && r) | p <- bools, q <- bools, r <- bools]
-- P \/ (Q \/ R) = (P \/ Q) \/ R             or is associative
or_assoc = and [(p || (q || r)) == (p || q) || r | p <- bools, q <- bools, r <- bools]
-- P /\ (Q \/ R) = (P /\ Q) \/ (P /\ R)      and distributes over or
and_over_or = and [(p && (q || r)) == (p && q) || (p && r) | p <- bools, q <- bools, r <- bools]
-- P \/ (Q /\ R) = (P \/ Q) /\ (P \/ R)      or distributes over and
or_over_and = and [(p || (q && r)) == ((p || q) && (p || r)) | p <- bools, q <- bools, r <- bools]

-- P -> (Q /\ R) = (P -> Q) /\ (P -> R)      implies distributes over and
imp1 = and [(p <= (q && r)) == ((p <= q) && (p <= r)) | p <- bools, q <- bools, r <- bools]
-- (P \/ Q) -> R = (P -> R) /\ (Q -> R)
imp2 = and [((p || q) <= r) == ((p <= r) && (q <= r)) | p <- bools, q <- bools, r <- bools]
-- P -> (Q -> R) = (P /\ Q) -> R
imp3 = and [(p <= (q <= r)) == ((p && q) <= r) | p <- bools, q <- bools, r <- bools]


-- The exclusive-or (xor) operation is equivalent to the /= operator in Haskell
-- Which of the following properties of exclusive-or are true? Answer each by
-- supplying Haskell code to check.

-- xor is commutative (true)
xor_commun = and [(p /= q) == (q /= p) | p <- bools, q <- bools]
-- xor is associative (true)
xor_asso = and [(p /= (q /= r)) == ((p /= q) /= r) | p <- bools, q <- bools, r <- bools]
-- xor distributes over and (false)
xor_over_and = and [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <- bools]
-- xor distributes over or (false)
xor_or = and [(p /= (q || r)) == ((p /= q) || (p /= r)) | p <- bools, q <- bools, r <- bools]
-- and distributes over xor (true)
and_xor = and [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r <- bools]
-- or distributes over xor (false)
or_over_xor = and [(p || (q /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <- bools]
-- implies distributes over xor (false)
imply_over_xor = and [(p <= (q /= r)) == ((p <= q) /= (p <= r)) | p <- bools, q <- bools, r <- bools]


-- Translate each of the statements below, first, in a comment after "A: ",
-- into a logical statement involving forall, exists, /\, \/, ->, and not,
-- and then into Haskell code that checks ("brute force") whether the
-- statement is true. The universe of discourse in each case is u. Your
-- code should work with any universe u (try out several!), but here is
-- a particular one you can use (the order of elements shouldn't matter,
-- since both `and` and `or` are commutative):

u = [13,4,12,22,9,1,2,17,5]

-- I'll work the first example in two different ways

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) -> (n > 1)
prob1  = and [(n > 2) <= (n > 1) | n <- u]    -- direct solution
prob1' = and [n > 1 | n <- u, n > 2]          -- using a bounded quantifier

-- 2. Every number is either greater than 5 or less than 6
-- A: forall n, (n > 5) \/ (n < 6)
prob2 = and [(n > 5) || (n < 6) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall n, forall s, (n -> s) \/ (s -> n)
prob3 = and [(n <= s) || (s <= n) | n <- u, s <- u]

-- 4. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: forall odd n, (exists even s, (n < s))
prob4 = and [(or [n < s | s <- (filter(even) u)]) | n <- (filter(odd) u )]

-- 5. For every even number, there is a greater odd number
-- A: forall even s, (exists odd s, (n < s) False
prob5 = and [(or [n < s | s <- (filter(odd) u)]) | n <- (filter(even) u )]

-- 6. There are two odd numbers that add up to 6
-- A: exists odd n, (exists odd s, (n + s = 6))
prob6 = or [(n + s ==6) | n <- (filter(\m ->odd m) u), s <- (filter(\m ->odd m) u)]

-- 7. There are two even numbers that add up to 20
-- A: exists even n, (exists even s, (n + s = 20)) False
prob7 = or [(n + s ==20) | n <- (filter(\m ->even m) u), s <- (filter(\m ->even m) u)]

-- 8. There is a largest number (i.e., there is a number that is >= all numbers)
-- A: exists n, (forall s, n >= s)
prob8 = or [(or [n >= s | s <- u]) | n <- u]

-- 9. For every two different numbers, there is a third number in between.
-- A: forall n, forall s, (exists m, (n < m < s) \/ (s < m < n))
prob9 = undefined

-- 10. For every number, there is a different number such that there are no
--     numbers between these two.
-- A: forall n, (exists s, n >= m)
prob10 = and [(or[(abs (s - n) ==1) | s <- u]) | n <- u]


