import Prelude() -- Do not import prelude stl via ()
import MiniPrelude -- import MiniPrelude library from prof
import Test.QuickCheck

-- This is the program for practice 0

-- q1
myeven :: Int -> Bool
myeven x = x `mod` 2 == 0

-- q2
area :: Float -> Float
area r = (22/7) *. r *. r

-- q3
payment :: Int -> Int
payment week = let days = 5 * week
                   hours = 8 * days
                   in 130 * hours


-- q5
smaller :: Int -> Int -> Int
smaller x y = 
    if x <= y then x else y

-- ghci Work.hs
-- :i smaller will return the type of the function
--
st3 = smaller 3
-- st3 7 = (smaller 3) 7 = if 3 <= 7 then 3 else 7
-- = if True them 3 else 7

-- q6
poly :: Int -> Int -> Int -> Int -> Int
poly a b c x = 
    a * x * x + b * x + c

poly1 :: Int -> Int
poly1 x =
    poly 1 2 1 x

poly2 :: Int -> Int -> Int -> Int
poly2 a b c = poly a b c 2

-- q7
square :: Int -> Int
square x = x * x

quad :: Int -> Int
quad x = twice square x
-- or only wirte quad = twice square
-- 
-- twice :: ...


-- Notice: in haskell, the variable with first capital means the determined type, and the one with first lower case means the variable

-- q8
twice :: (a -> a) -> (a -> a)
twice f x = f (f x)
-- or twice f = f . f
-- e.g. quad 3 = twice square 3 = square (square 3) = ...
--

inc :: Int -> Int
inc x = 1 + x

-- if we write (1+) in haskell, it will be defined as a function, therefore, if we find the type for (1+ ), it will be a int -> int function

-- q9
forktimes f g x = f x * g x
quadratic = forktimes (1+) (2+)
-- (+1) is also available

lift2 h f g x = h(f x)(g x)
quadratic2 = lift2 (*)(1+) (2+)
-- (*) means the parameter psssing into the function lift2, note that brackets is necessary in the haskell.
-- also, in this case, we can rewrite forktimes as
-- forktimes = lift2 (*)
--
