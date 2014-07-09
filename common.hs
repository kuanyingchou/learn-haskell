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
last' (x:xs) = last' xs

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


