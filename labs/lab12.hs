-- Lab 12: Partitions

-- Sets of Ints as lists. Invariant: sets as lists always has no duplicates
import Data.List
type Set = [Int]

-- Relations on sets of Ints. Invariant: no duplicates
type Rel = [(Int,Int)]

-- Universe (your code should work with any non-empty universe)
u = [1..8]

-- set criteria: not trivial, must contain all of universe, must not have any duplicates
-- equivalence relations: 

-- A partition of u is a set of blocks, each of which is a set of elements
-- of u, which satisfies certain conditions (nontrivial, total, disjoint).
-- For example, here is the partition of u corresponding to equivalence mod 3:
eqmod3_part :: [[Int]]
eqmod3_part = [[1,4,7], [2,5,8], [3,6]]

trivial :: [[Int]] -> Bool
trivial cs = or [null (ds) | ds <- cs]
-- empty block = True, we want nontrivial

complete :: [[Int]] -> Bool
complete bs = and [or [x `elem` ds | ds <- bs] | x <- u]
-- must have entire universe

disjoint :: [[Int]] -> Bool
disjoint xs = and [and [not (x `elem` ys) | x <- bs] | bs <- xs, ys <- xs, ys /= bs]
-- no duplicates are allowed


-- Write a function, part, that tests whether a list of lists is a partition
-- of u, including that each block is a set (i.e., has no duplicates)
-- is nontrivial, total, disjoint(intersection is empty)
part :: [[Int]] -> Bool
part bs = not(trivial bs) && complete bs && disjoint bs


-- Write a function, eq2part, that takes an equivalence relation on u as input
-- and returns the associated partition of u. You can assume that the input is
-- really an equivalence relation on u.
eq2part :: Rel -> [[Int]]
eq2part rs = nub [[d | (c,d) <- rs, a == c] | (a,b) <- rs]
-- eq2part [(1,1),(1,2),(2,1),(2,2),(3,3)] = [[1,2],[3]]


-- Write a function part2eq that takes a partition of u as input and returns
-- the associated equivalence relation on u. You can assume that the argument
-- is really a partition of u.
part2eq :: [[Int]] -> Rel
part2eq bs = [(x,y)| cs <- bs, x <- cs, y <- cs]
-- part2eq [[1,2],[3]] = [(1,1),(1,2),(2,1),(2,2),(3,3)]


-- Write a function, kernel, that takes a function on u as input (given as
-- a list of pairs), and returns the associated equivalence relation on u.
-- You can assume that the input is really a function on u.
kernel :: [(Int,Int)] -> Rel
kernel rs = undefined
-- kernel? [(1,2),(3,3)] = [(1,1),(1,2),(2,1),(2,2),(3,3)]?