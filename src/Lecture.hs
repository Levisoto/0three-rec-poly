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

executeOnScreen = do
   print (addOneToAll myIntList)
   print (absAll  myIntList)
   print (squareAll  myIntList)
   print (filterIntList even  myIntList)

-- That's a new way to learn I want to try
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
data List t = E
            | C t (List t)
            deriving (Eq, Show)

-- lst1 :: List Int
-- lst1 = C 3 (C 5 (C 2 E))

-- lst2 :: List Char
-- lst2 = C 'x' (C 'y' (C 'z' E))

-- lst3 :: List Bool
-- lst3 = C True (C False E)

-- maptlist :: (a -> a) -> (List a) -> (List a)
-- maptlist _ E = E
-- maptlist f (C x xs) = C (f x) (maptlist f xs)

-- addOnePoly :: Int -> Int
-- addOnePoly x = x + 1

-- replaceChar :: Char -> Char
-- replaceChar _ = 'l'

-------------------------------------------------------------
-------------------------------------------------------------
-- TOOL's
fromListTOrecPtt :: [a] -> (List a)
fromListTOrecPtt [] = E
fromListTOrecPtt (x:xs) = C x (fromListTOrecPtt xs)

takeValue :: (List a) -> Int -> (List a)
takeValue list 1    = list
takeValue E _       = E
takeValue (C _ E) _ = E
takeValue list i 
  | lengthList list < i = E
  | otherwise           = C m (takeValue mn i)
  where
    (C m mn) = applyNtimes (i-1) ignore list

ignore :: (List a) -> (List a)
ignore E       = E
ignore (C _ E) = E
ignore (C _ m) = m

takeNum :: (List a) -> Char
takeNum E = ' '
takeNum (C _ _) = '*'

lengthList :: (List a) -> Int
lengthList = length.fromrecPttTOList

fromrecPttTOList :: (List a) -> [a]
fromrecPttTOList E = []
fromrecPttTOList (C x xs) = (x:fromrecPttTOList xs)


applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

-------------------------------------------------------------
-------------------------------------------------------------
 
skips :: [a] -> [[a]]
skips m = [fromrecPttTOList $ takeValue toList i | i <- [1..length m]]
  where 
    toList = fromListTOrecPtt m

localMaxima :: [Integer] -> [Integer]
localMaxima list = fromrecPttTOList (inOrder list2) 
  where
    list2                        = fromListTOrecPtt list
    inOrder (C _ E)              = E
    inOrder (C _ (C _ E))        = E
    inOrder (C x (C y (C z xs)))
      | (y > x) && (y > z) = (C y (inOrder (C z xs)))
      | otherwise          = inOrder (C y (C z xs))

histogram :: [Integer] -> String
histogram list = (unlines list4) ++ "==========\n0123456789\n"
  where
    list2 = fromListTOrecPtt list
    list3 = [findNum (==i) list2 | i <- [0..9]]
    long  = maximum $ map lengthList list3
    list4 = [map takeNum $ map (applyNtimes (i-1) ignore) list3 | i <- [long,long - 1..2]] ++ [map takeNum list3]

findNum ::(a -> Bool) -> (List a) -> (List a)
findNum f E = E
findNum f (C x xs)
  | f x = C x (findNum f xs)
  | otherwise = findNum f xs

-- this part is to learn "Maybe from library Prelude"

-- Partial functions are which have some value for which this function will crash
-- The opossit of this are tottal functions
-- For example
---- /show
emptyStringList :: [String]
emptyStringList = []
-- show
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

main = print (safeHead emptyStringList, safeHead ["hello"])

data NonEmptyList a = NEL a [a]
                    deriving Show

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNEL :: [a] -> Maybe (NonEmptyList a)
listToNEL []     = Nothing
listToNEL (x:xs) = Just (NEL x xs)

headNEL :: NonEmptyList a -> a
headNEL (NEL x _) = x

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ xs) = xs
