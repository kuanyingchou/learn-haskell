-- 99 questions from https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x : (init' xs)

-- p2
butLast' :: [a] -> a
butLast' xs = last' (init' xs) -- last . init

-- p3
myElementAt :: [a] -> Int -> a
myElementAt [] _ = error "empty list"
myElementAt (x:xs) i
    | i > 1 = myElementAt xs (i-1)
    | i == 1 = x
    | otherwise = error "index less than 1"

-- p4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

-- p5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) [] -- !

-- p6
myIsPalindrome :: (Eq a) => [a] -> Bool
myIsPalindrome xs 
    | same xs (myReverse xs) = True
    | otherwise = False
    where 
    same (x:xs) (y:ys)
        | x == y = True && (same xs ys)
        | otherwise = False
    same [] [] = True

-- p7 !
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
    

{-
zip' :: ([a], [b]) -> [(a, b)]
zip' ([], _) = []
zip' (_, []) = []
zip' (x:xs, y:ys) = (x, y) : zip' (xs, ys)
-}

-- p8
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:y:xs) 
    | x == y = rest
    | otherwise = x : rest 
    where rest = myCompress (y:xs)

-- p9
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = [] -- [] is a [[a]]
myPack all@(x:xs) = first : myPack rest
        where (first, rest) = break (/=x) all

-- p10
myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode xs = [(length x, head x)| x<-(myPack xs)]

-- p11
data Entry a = Single a | Multiple Int a deriving Show
myEncode' :: (Eq a) => [a] -> [Entry a]
myEncode' [] = []
myEncode' xs = [if len == 1 then Single fir else Multiple len fir 
    | x<-(myPack xs), let len = length x, let fir = head x]

-- p12
myDecode' :: [Entry a] -> [a]
myDecode' [] = []
myDecode' ((Single a):xs) = a : myDecode' xs
myDecode' ((Multiple b c):xs) = (replicate b c) ++ myDecode' xs    
