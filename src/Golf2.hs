module Golf2 where

data List t = E
            | C t (List t)
            deriving (Eq, Show)

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
