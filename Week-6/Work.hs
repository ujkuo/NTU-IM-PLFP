-- 2020.10.22

import Prelude()
import MiniPrelude


dropWhile' p (x:xs) = 
    if p x then dropWhile' p xs
           else x : xs

take' :: Nat -> List a -> List a
take' 0      xs     = []
take' n  []     = []
take' n  (x:xs) = x : take' (n-1) xs 
-- define a function on nature number
-- take' 2 [3,4,5] = [3,4]
-- take' 0 [3,4,5] = []
-- take' 7 [3,4,5] = [3,4,5]


drop' :: Nat -> List a -> List a
drop' 0     xs      = xs
drop' n []    = []
drop' n (x:xs)  = drop' (n-1) xs
-- drop' 1 [3,4,5] = [4,5]


inits :: List a -> List (List a)
inits [] = [[]] -- base case
inits (x:xs) = [] : map(x:) (inits xs)

-- inits xs = [take n xs | n <- [0..length xs]]

--
-- inits [1,2,3] = [[],[1],[1,2],[1,2,3]]
-- inits [1,2,3,4] = [[],[1],[1,2],[1,2,3],[1,2,3,4]]
-- => inits(1:[2,3,4]) = [[],[1],[1,2],[1,2,3],[1,2,3,4]]
-- inits [2,3,4] = [[],[2],[2,3],[2,3,4]]
--

tails :: List a -> List (List a)
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

-- tails[2,3,4] = [[2,3,4],[3,4],[4],[]]


-- practice 3 p.6
fan :: a -> List a -> List (List a)
-- if you have no idea, divide into these two cases below
-- fan y [] = ???
-- fan y (x:xs) = ??? fan y xs ???
--
fan y [] = [[y]] -- insert y into list
fan y (x:xs) = (y:x:xs) : map (x:) (fan y xs)
--          -- paranthesis here is necessary
-- in this case, (x:xs) means [1,2,3,4], x is 1 in list and xs is the remaining one
-- (y:x:xs) indicate that insert 5 before [1,2,3,4], map(x:) means add [1] in the remaining part
-- or another method using drop and take

fan' y xs = map(\n -> take n xs ++ (y:[]) ++ drop n xs) [0..length xs]
-- ++ is called append'
--
perms :: List a -> List (List a)
perms [] = [[]] -- base case
perms (x:xs) = concat(map (fan x) (perms xs))
-- the reason why we need to use concat here is on the picture in cellphone
-- type of map: given a function (c -> d), given List c, return List d
-- i.e. (c -> d) -> List c -> List d
-- and that's why we need to use concat function to divide the []

-- tree on worksheet for 3, p.5

data Tree a = NULL | Node a (Tree a) (Tree a)
 deriving Show 
-- this tree consists of a node and two sub-tree
-- the space before deriving is important which is to represent
--
t :: Tree Int
t = Node 4 (Node 3 NULL NULL) (Node 7 (Node 2 NULL NULL) NULL)

sumT :: Tree Int -> Int
sumT NULL = 0
sumT (Node x t u) = x + sumT t + sumT u
-- sumT(t) is also available here

-- P :: Tree a -> Bool
-- To prove that for all t, P(t) is true



-- Practicals 3 p.3

filter' :: (a -> Bool) -> List a -> List a
filter' p [] = []
filter' p (x:xs)  | p x = x : filter' p xs
                  | otherwise = filter' p xs

-- Proof. Induction on xs
-- Case xs := []
-- filter p (map f [])
-- = filter p []
-- = map f []
-- = map f (filter (p . f))[]
--
-- Case xs := x : xs
-- filter p (map f (x:xs))
-- = filter p (f x : map f xs) // definition of map
-- = if p (f x) then f x : filter p (map f xs)
--   else filter p (map f xs)
-- =   {induction}
--   if p (f x) then f x : map f (filter (p . f) xs)
--   else map f (filter (p . f) xs)
-- = if p (f x) then map f (x : (filter (p . f) xs)) // definition of map
--   else map f (filter (p . f) xs)
-- = {if-else distribution}
--   map f (if p (f x) then x : filter (p . f) xs
--          else filter (p . f) xs)
-- = map f (if (p . f) x then x : filter (p . f) xs
--          else filter (p . f) xs)
-- = map f (filter (p . f) (x : xs))


-- Practicals 3 p.4
-- To prove that f (if q then e1 else e2) ?
-- => Let q := inf, f = 3, fx = 3
-- if q then f e1 else f e2 => non-ternination
