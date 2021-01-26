--Jamie Terry 
--Haskell HW 2

-- Problem 1:
-- Determine the types of "3", "even", and "even 3". how do you determine the last one? 
-- 3 is type int 
-- even in type int to bool
-- bool, this is determined by the fact that the function is already given its int, 
-- so it will return a bool, and be type bool 

import Data.Char

--Problem 2:
--For each type, write a function with that type. 
--(a) (Float -> Float) -> Float 
funcA x = x 5
--treat x as a function, not an argument

--(b) Float -> (Float -> Float)
add :: Float -> (Float -> Float)
add x y = x+y

--(c) (Float -> Float) -> (Float -> Float)
funcC x y = x y


--Problem 3: 
--Write the ncopies function that works according to the follwoing example. 
--Code this function first without recursion and then with recursion 

ncopies j k = take j (repeat k) 

ncopiesRec j k = copiesHelper j k []
copiesHelper j k l 
      | j < 0            = []
      | j == 0           = l
      | otherwise        = copiesHelper (j-1) k (l ++ [k])

--Problem 4:
--Write a function diffs that returns a list of the differences between adjacent items. 
--So, diffs [3,5,6,8] returns [2,1,2]
diffs [] = []
diffs (x1:[]) = [] --only 1 element, tail is empty, can't do anything with this, so returns empty list 
diffs (x1:x2:xs) = (abs(x1 - x2)) : diffs (x2:xs) 


--Problem 5:
--Write a function groupByN with the type:
--    groupByN :: Int -> [a] -> [[a]]
--This function splits the given list in sub-lists, where the sublists have a given length. 
-- Only the last sub-list may be shorter.
--example: 
--    groupdByN 3 [1,2,3,4,5,6,7,8,9,10]
--    [[1,2,3], [4,5,6], [7,8,9], [10]]
groupByN y [] = []
groupByN y xs = take y xs : groupByN y (reverse $ take (length xs - y) $ reverse xs)



--problem 6:
--Design a way to represent straight lines in a cartesian system as internal data in Haskell (y=mx+b)and then write a 
--function that calculates the x-interceptor for a given line. Be sure to test using interesting cases!
type Slope = Float
type Yintercept = Float
type Line = (Slope, Yintercept)
type X = Float 
type Y = Float
type Point = (X,Y)

xIntercept :: Line -> Point
xIntercept (s, y) = (-y / s, 0)

--Problem 7: 
--Write a function which converts a string of digits into an int. You will need the following predefined function:
--    ord '1'    --> 49      first char in arg to its ascii code. 
--follow the following "pipeline" analysis when defining your function
--    "167" --> ['1', '6', '7']  --> [49, 54, 55] --> [1,6,7] --> [(1,100), (6, 10), (7, 1)] --> [100, 60, 7] --> 167

--please input the number to convert as a list of characters. ex. 167 needs to be input as ['1', '6', '7']
multiplyPair (x, y) = x * y

prob7A cs = prob7B (map ord cs)
prob7B is = prob7C (map (subtract 48) is)
prob7C ps = prob7D (reverse (zip (reverse ps) (zipWith (^) (repeat 10) [0, 1..] )))
prob7D qs = sum (map multiplyPair qs)
