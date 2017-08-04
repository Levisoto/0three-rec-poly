module Golf where

--data List t = E

skips :: [a] -> [[a]]
skips list = [each i list | i <- [1..length list]]

each :: Int -> [a] -> [a]
each n list = [list !! i | i <- [n-1,n + n - 1..length list - 1]]

--localMaxima :: [Integer] -> [Integer]
--localMaxima (x:y:[]) = []
--localMaxima (x:[]) = []
--localMaxima [] = []
--localMaxima list
--  | lookingfor m = (m !! 1):(localMaxima $ tail list)
--  | otherwise = localMaxima $ tail list
    --where
      --m = take 3 list
--
----lookingfor :: [Integer] -> Bool
--lookingfor (t1:t:t2:xs)
--  | t > t1 && t > t2 = True
--  | otherwise = False

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram list = (unlines $ map getString $ getList list) ++ "==========\n0123456789\n"

getList :: [Integer] -> [[Integer]]
getList list = [map (\ x -> if (length x >= (i+1)) then (x !! (i))  else (-1)) mlist | i <- [maxlist-1,maxlist-2..0]]
  where
    mlist = map (\ x -> filter (==x) list) [0..9]
    maxlist = maximum $ map (\ x -> length x) mlist

getString :: [Integer] -> String
getString list = [if (elem x [0..9]) then 'x' else ' ' | x <- list]

{--Creating a new Datatype (First attempt)
data List t = Empty
            | Cons t (List t)
            deriving Show

addNew :: Integer -> List Integer -> List Integer
addNew x Empty = Cons x Empty
addNew t (Cons t1 right)
  | t <= t1 = Cons t (Cons t1 right)
  | otherwise = Cons t1 (addNew t right)

inOrder :: [Integer] -> List Integer
inOrder [] = Empty
inOrder m = foldr addNew Empty m

getIn :: List Integer -> [Integer]
getIn Empty = []
getIn (Cons t right) = [t]++getIn(right)

addList :: Integer -> [Integer] -> [Integer]
addList t [] = [t]
addList t (x:xs)
  | t <= x = (t:x:xs)
  | otherwise = (x:addList t xs)

inOrderList :: [Integer] -> [Integer]
inOrderList = foldr addList []
-}
