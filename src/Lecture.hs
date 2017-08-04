module Lecture where

data IntList = Empty
             | Cons Int IntList
        deriving Show

--Show
{-
addOneToAll :: IntList -> IntList
addOneToAll Empty = Empty
addOneToAll (Cons x xs) = Cons (x+1) (addOneToAll xs)

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)


squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)
-}

--IMPLEMENTING RECURSIVE PATTERNS (Functions to pass other Functions)
mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)
--------------------------------------------------------------

addOne x = x + 1
square x = x*x

addOneToAll xs = mapIntList addOne xs
absAll xs = mapIntList abs xs
squareAll xs = mapIntList square xs

--IMPLEMENTING FILTER FUNCTION TO PASS TO OTHER FUNCTION
filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList p (Cons x xs)
  | p x = Cons x (filterIntList p xs)
  | otherwise = filterIntList p xs


myIntList = Cons 2 (Cons (-3) (Cons 5 Empty))

--main = print (addOneToAll myIntList)
--main = print (absAll  myIntList)
--main = print (squareAll  myIntList)
main = print (filterIntList even  myIntList)
