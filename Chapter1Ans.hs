module Chapter1Ans where

{- |
=X= Task 1
-----------------
>>> :t True
True :: Bool

>>> :t 'a'
'a' :: Char

>>> :t 42
42 :: Num a => a

>>> :t (True, 'x')
(True, 'x') :: (Bool, Char)

>>> :t not
not :: Bool -> Bool

>>> :t (&&)
(&&) :: Bool -> Bool -> Bool

>>> :t (+)
(+) :: Num a => a -> a -> a

>>> :t max
max :: Ord a => a -> a -> a
-}

{- |
=X= Task 2
-----------------
>>> 1 + 2
3

>>> 10 - 15
-5

>>> 10 - (-5)
15

>>> (3 + 5) < 10
True

>>> True && False
False

>>> 10 < 20 || 20 < 5
True

>>> 2 ^ 10
1024

>>> not False
True 

>>> div 20 3
6

>>> mod 20 3
2

>>> max 4 10
10 

>>> min 5 (max 1 2)
2

>>> max (min 1 10) (min 5 7)
5

-}


{- 
=X= Task 3
-----------------
specify types

>>> swuareSum 3 4
49
-}
squareSum :: Integer -> Integer -> Integer
squareSum x y = (x + y) * (x + y)

{-
=X= Task 4
-----------------
Implement the fuction that takes an integer value and returns the next 'Int'.

>>> next 10
11
>>> next (-4)
-3

-}
next :: Int -> Int
next x = x + 1


{-
=X= Task 5
-----------------
Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2

-}
lastDigit :: Integer -> Integer
lastDigit n = mod (abs n) 10


{-
=X= Task 6
-----------------
Implement a function, that takes two numbers and returns the closer one to zero

>>> closestToZero 10 5
5
>>> closestToZero (-7) 3
3

-}
closestToZero :: Int -> Int -> Int
closestToZero x y = if abs x < abs y then x else y


{-
=X= Task 7
-----------------
Write a function that returns the middle number among three given nubmers.

>>> mid 3 1 2
2

-}
mid :: Integer -> Integer -> Integer -> Integer 
mid x y z 
    | (min x y) <= z && (max x y) >= z = z
    | (min y z) <= x && (max y z) >= x = x 
    | (min z x) <= y && (max z x) >= y = y 


{-
=X= Task 8
-----------------
Implement a function that checks whether a given character is a vowel.

>>> isVowel 'a'
True
>>> isVowel 'x'
False

-}
isVowel :: Char -> Bool
isVowel c 
    | c == 'a' = True
    | c == 'e' = True
    | c == 'i' = True
    | c == 'o' = True
    | c == 'u' = True
    | c == 'y' = True
    | otherwise = False

{-
=x= Task 9 
-----------------

Implement a function taht returns the sum of the last two digits of a number.

>>> sumLast2 42
6
>>> sumLast2 134
7
>>> sumLast2 1
1

-}
sumLast2 :: Integer -> Integer
sumLast2 n =
    let digit1 = getPlace (abs n) 1
        digit2 = getPlace (abs n) 10
    in digit1 + digit2
    where
        cutAtPlace :: Integer -> Integer -> Integer
        cutAtPlace n p = mod n p
        
        getPlace :: Integer -> Integer -> Integer
        getPlace n p = div (cutAtPlace n (p * 10) - cutAtPlace n p) p

    

{-
=X= Task 10*
CHALLENGE
Implement a function that returns the first digit of a given number.

>>> firstDigit 230
2 
>>> firstDigit 5623
5

-}

firstDigit :: Integer -> Integer
firstDigit n = 
    let maxPlace = findMaxPlace (abs n) 1
    in cutBottom (abs n) maxPlace
    where
        findMaxPlace :: Integer -> Integer -> Integer
        findMaxPlace n p = 
            if not (mod n (p * 10) == n)
            then findMaxPlace n (p * 10)
            else p

        cutBottom :: Integer -> Integer -> Integer
        cutBottom n p = div (n - mod n p) p
               

