-- Lab 13: Partial Orders

type Set = [Int]
type Rel = [(Int,Int)]

u :: Set
u = [1..8]


-- For your information, here are tests for reflexivity, symmetry,
-- transitivity, and antisymmetry, along with a test for partial order
refl :: Rel -> Bool
refl ps = and [elem (x,x) ps | x <- u]

symm :: Rel -> Bool
symm ps = and [elem (y,x) ps | (x,y) <- ps]

trans :: Rel -> Bool
trans ps = and [elem (x,z) ps | (x,y1) <- ps, (y2,z) <- ps, y1 == y2]

antisymm :: Rel -> Bool
antisymm ps = and [not (elem (y,x) ps) | (x,y) <- ps, x /= y]

poset :: Rel -> Bool
poset ps = refl ps && trans ps && antisymm ps


-- A few example partial orders on u (develop some more yourself!)

eq = [(i,i) | i <- u]
leq = [(i,j) | i <- u, j <- [i..8]]
divis = [(i,j) | (i,j) <- leq, j `mod` i == 0]
two_by_four = [(i,j) | (i,j) <- leq, even i <= even j]


---- Lab 13 begins here ----

-- Note: all functions below can assume that poset ps == True (you don't need
-- to check this).

-- Maybe Int:: Just n
-- Nothing

-- least leq [2,3,4] = Just 1
-- Least element of a subset of a poset (if it exists)
-- least ps xs == Justa, if a is the least element of xs
-- least ps xs == Nothing, if xs has no least element
least :: Rel -> Set -> Maybe Int
least ps xs 
            | y > x = Just x
            | y < x = Just y
            | otherwise Nothing
            where y = minimum [min a b | (a,b) <- ps]
                 x = minimum xs

-- Greatest element of a subset of a poset (if it exists)
-- greatest ps xs == Just a, if a is the greatest element of xs
-- greatest ps xs == Nothing, if xs has no greatest element
greatest :: Rel -> Set -> Maybe Int
greatest ps xs 
               | y > x = Just y
               | y < x = Just x
               | otherwise Nothing
               where y = maximum [min a b | (a,b) <- ps]
                    x = maximum xs

-- Least upper bound of two elements of a poset (if it exists)
lub :: Rel -> Int -> Int -> Maybe Int
lub ps x y = undefined

-- Greatest lower bound of two elements of a poset (if it exists)
glb :: Rel -> Int -> Int -> Maybe Int
glb ps x y = undefined

-- linear ps == True if ps is a linear order
linear :: Rel -> Bool
linear ps = undefined

-- lattice ps == True if ps is a lattice
lattice :: Rel -> Bool
lattice ps = undefined

-- Coverings of an element in a poset
coverings :: Rel -> Int -> Set
coverings ps x = undefined

-- Minimal elements of a subset of a poset
-- Least element that has nothing below it, eq
minimal :: Rel -> Set -> Set
minimal ps xs = undefined


-- Recursion
-- Topological sorting of a subset of a poset
topsort :: Rel -> Set -> Set
topsort ps xs = undefined
