head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = x : init' xs
    
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

take' :: Integer -> [a] -> [a]
take' _ [] = []
take' n all@(x:xs) 
    | n > 0 = x : take' (n-1) xs
    | otherwise = []

drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' n all@(x:xs) 
    | n > 0 = drop' (n-1) xs
    | otherwise = all

bangBang :: [a] -> Integer -> a
bangBang [] _ = error "index out of bounds"
bangBang (x:xs) n 
    | n > 0 = xs `bangBang` (n-1)
    | n == 0 = x
    | otherwise = error "negative index"

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

null' :: [a] -> Bool
null' [] = True
null' xs = False

min' :: (Ord a) => a -> a -> a
min' x y = if x <= y then x else y

max' :: (Ord a) => a -> a -> a
max' x y = if x >= y then x else y

succ' :: Integer -> Integer
succ' i = i + 1

sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' [x] = [x]
sort' (x:xs) = (sort' left) ++ [x] ++ (sort' right)
    where left = [e | e<-xs, e<=x]
          right = [e | e<-xs, e>x]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- flip' :: (a -> a -> b) -> a -> a -> b
-- flip' f a b = f b a
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : rest
    | otherwise = rest
    where rest = filter' f xs

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' a [xs] = [xs]
intersperse' a (x:xs) = x : a : intersperse' a xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs : reverse' (init xs) -- reverse' xs ++ [x]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' [] xs = concat' xs
intercalate' x ys = concat' $ intersperse' x ys

-- [[1, 2, 3], [4, 5, 6]]
-- =>
-- [[1, 4], [2, 5], [3, 6]]

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' all@(x:xs) = 
        [[x !! (fromIntegral i) | x <- all ] | i <- [0..(length' x)-1] ]

concatMap' :: (a -> [a]) -> [a] -> [a]
concatMap' _ [] = []
concatMap' f xs = concat' $ map f xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) 
    | x = and' xs
    | otherwise = False

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) 
    | x = True
    | otherwise = or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) 
    | f x = True
    | otherwise = any' f xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) 
    | not (f x) = False
    | otherwise = all' f xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- is there sth similar in the standard lib?
takeFrom' :: Integer -> [a] -> [a]
takeFrom' _ [] = []
takeFrom' x all@(y:ys)
    | x > 0 = takeFrom' (x-1) ys
    | otherwise = all

splitAt' :: Integer -> [a] -> ([a], [a])
splitAt' i xs = (take' i xs, takeFrom' i xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) 
    | f x = x : takeWhile' f xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f all@(x:xs)
    | f x = dropWhile' f xs
    | otherwise = all

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f xs = (takeWhile' f xs, dropWhile' f xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f xs = span' (not . f) xs

-- fold' (+) 0 [1..3]
-- = 0 + 1 + 2 + 3 = 6
foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' _ a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "empty list"
foldl1' f (x:xs) = foldl' f x xs

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f a xs = foldl1' (flip' f) rev
    where rev = a : (reverse' xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs = foldr' f (last xs) (init xs)

