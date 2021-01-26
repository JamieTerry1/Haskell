--Jamie Terry
--Haskell HW 1 

--Code the last function for lists using the built in reverse function for lists. 
--the last function returns the last element of a list 
reverseList [] = 0
reverseList (x:xs) = head $ reverse (x:xs) 

--Code the init function for lists using the built in reverse function for lists
--The init function returns the list of all but the last element of a list
initList [] = []
initList (x:xs) = reverse $ tail $ reverse (x:xs)

--Code a function that returns the first and last elements of a list as a 2-tuple (a pair)
--Give a type for the function
firstLastList [] = (0,0)
firstLastList (x:xs) = (x ,reverseList (x:xs) )

--Code the function rangeProduct that computes according to the given example
rangeProductNoRec :: Integer -> Integer -> Integer
rangeProductNoRec m n 
    | m > n     = 0
    | m == n    = n
    | otherwise = product[m..n]

rangeProduct m n 
    | m > n     = 0
    | m == n    = n
    | otherwise = m * rangeProduct (m + 1) n

--Code a version of the factorial function that uses your rangeProduct function 
factorial x = rangeProduct 1 x

--Write the power :: Integer -> Integer -> Integer function for integers without using the exponentiation operator.
--first code without recursion and then with recursion
powerNoRec j k = product (take k (repeat j)) 

power j k 
    | k == 0       = 1
    | otherwise    = powerHelper j (k - 1) j 

powerHelper j k t 
    | k == 0       = t
    | otherwise    = powerHelper j (k - 1) (t * j)
     
