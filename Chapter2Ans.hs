{-
Chapter 2
-}

module Chapter2Ans where

{-
=X= Task 1
-------------------
Explore lists by checking types of various list expressions and 
functions in GHCi and insert thye correspongin resulting output below:

List of booleans:
>>> :t [True, False]
[True, False] :: [Bool]

String is a list of characters:
>>> :t "some string"
"some string" :: String

Empty list:
>>> :t []
[] :: [a]

Append two lists:
>>> :t (++)
(++) :: [a] -> [a] -> [a]

Prepend an element at the beginning of a list
>>> :t (:)
(:) :: a -> [a] -> [a]

Reverse a list:
>>> :t reverse
reverse :: [a] -> [a]

Take first N elements of a list 
>>> :t take
take :: Int -> [a] -> [a]

Create a list from N same elements:
>>> :t replicate
replicate :: Int -> a -> [a]

Split a string by line breaks:
>>> :t lines
lines :: String -> [String]

Join a list of strings with line breaks:
>>> :t unlines
unlines :: [String] -> String

-}


{-
=X= Task 2
-------------------
To understand the list data type better, it is also beneficial to play with 
list expressions in REPL.

Evaluate the following expressions in GHCi and insert the answer

>>> [10, 2] ++ [3, 1, 5]
[10,2,3,1,5]

>>> [] ++ [1, 4]
[1,4]

>>> 3 : [1, 2]
[3,1,2]

>>> 4 : 2 : [5, 10]
[4,2,5,10]

>>> [1 .. 10]
[1,2,3,4,5,6,7,8,9,10]

>>> [10 .. 1]
[]

>>> [10, 9 .. 1]
[10,9,8,7,6,5,4,3,2,1]

>>> length [4, 10, 5]
3

>>> replicate 5 True
[True,True,True,True,True]

>>> take 5 "Hello, World!"
"Hello"

>>> drop 5 "Hello, World!"
", World!"

>>> zip "abc" [1, 2, 3]
[('a',1),('b',2),('c',3)]

>>> words "Hello     Haskell     World!"
["Hello", "Haskell", "World!"]

-}


{-
=X= Task 3
-------------------
Let's write our first function to process lists in Haskell! Your first
implementation task is to write a function that returns all elements 
of a list between two given positions inclusicve (starting from zero)

>>> subList 3 5 [1 .. 10]
[4,5,6]

>>> subList 3 0 [True,False,False,True,False]
[]

-}
subList :: Int -> Int -> [a] -> [a]
subList bottom top list 
    | top < 0 || bottom < 0 = []      -- if either index param is negative, return []
    | top < bottom = []               -- if the max bounds is smaller than our min, return []
    | top == bottom = [list !! top]   -- if both index params are the same return [index]
    | otherwise =                     -- if none of these shortcuts are needed, do things normally
    take ((top - bottom) + 1) (drop bottom list)
     

{-
=X= Task 4
-------------------
Implement a function that returns only the first half of a given list.

>>> firstHalf [3,4,1,2]
[3,4]

>>> firstHalf "bca"
"b"

-}
firstHalf :: [a] -> [a]
firstHalf list =
    let listLen = length list
        halfLen = div listLen 2 
    in take halfLen list


{-
=X= Task 5
-------------------
Implement a function that checks whether the third element of a list
is the number 42

>>> isThird42 [1,2,42,10]
True 

>>> isThird42 [42,42,0,42]
False


-}
isThird42 :: [Int] -> Bool
isThird42 (_:_:42:_) = True
isThird42 _ = False 

{-
=X= Task 6
-------------------
Implement a function that dyuplicates each element of the list

>>> duplicate [3,1,2]
[3,3,1,1,2,2]

>>> duplicate "acab"
"aaccaabb"


-}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs


{-
=X= Task 7
-------------------
Write a function that takes elements of a list only in even positions.

>>> takeEven [2,1,3,5,4]
[2,3,4]


-}
takeEven :: [a] -> [a]
takeEven [] = []        -- recursion break, odd list length
takeEven (x:[]) = [x]   -- recursion break, even list length
takeEven (x:_:xs) = x : takeEven xs


{-
=X= Task 8
-------------------
Implement a function that repeats each element as many times as the 
value of the element itself

>>> smartReplicate [3,1,2]
[3,3,3,1,2,2]

-}
smartReplicate :: [Int] -> [Int]
smartReplicate [] = []
smartReplicate (x:xs) = replicate x x ++ smartReplicate xs


{-
=X= Task 9
-------------------
Implement a function that takes a number, a list of lists and returns
the list with only those lists that contain a paassed element.

>>> contains 3 [[1,2,3,4,5],[2,0],[3,4]]
[[1,2,3,4,5],[3,4]]

-}
contains :: Eq a => a -> [[a]] -> [[a]]
contains i l = filter (elem i) l


{- 
=X= Task 10 
-------------------
Let's now try to eta-reduce sonme of the functions and ensure that we
masterd the skill of eta-reducing.

-}

-- eta-reduction is gross. it apears to be faster and mainly used 
-- for optimization. higher speeds, lower readability(ie gross)

divideTenBy :: Int -> Int
divideTenBy = div 10

listElementsLessThan :: Ord a => a -> [a] -> [a]
listElementsLessThan x = filter (< x)

pairMul :: Num a => [a] -> [a] -> [a]
pairMul = zipWith (*)


{- 
=X= Task 11
-------------------
Implement a function to rater a given finite list by N elements. Try
to do it more efficently than rotating by a single element N times.

On invalid input (negative rotation coefficient) it should return an empty
list.

>>> rotate 1 [1,2,3,4]
[2,3,4,1]
>>> rotate 3 [1,2,3,4]
[4,1,2,3]

-}
rotate :: Int -> [a] -> [a]
rotate n l 
    | n < 0 = []
    | otherwise = 
    let listLen = length l
        listCycle = cycle l
    in take listLen (drop n listCycle)
        


{- 
=X= Task 12
-------------------
Implement the reversing function that take3s a list and reverses it.
without using the built in reverse function.

>>> rewind [1 .. 5]
[5,4,3,2,1]

-}
rewind :: [a] -> [a]
rewind l = rewindLoop (length l - 1) l
    where
        rewindLoop:: Int -> [a] -> [a]
        rewindLoop i ls = 
            if i >= 0 
            then ls !! i : rewindLoop (i - 1) ls 
            else []


