-- 99 questions from https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
import System.Random

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

-- p13
getValue :: Entry a -> a
getValue (Single x) = x
getValue (Multiple _ x) = x

getCount :: Entry a -> Int
getCount (Single x) = 1
getCount (Multiple n _) = n

myEncodeDirect :: (Eq a) => [a] -> [Entry a]
myEncodeDirect xs = 
    foldl 
    (\acc x -> if length acc == 0 then 
        [Single x]
    else if getValue (last acc) == x then 
        init acc ++ [Multiple ((getCount (last acc))+1) (getValue (last acc))]
    else 
        acc ++ [Single x]
    ) [] xs

-- p14
myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x:xs) = x : x : myDuplicate xs

-- p15
myReplicate :: [a] -> Int -> [a]
myReplicate [] _ = []
myReplicate xs n = concat [replicate n x | x<-xs]

-- p16
myDropEvery :: [a] -> Int -> [a]
myDropEvery xs n = concat $ zipWith drop xs (concat $ repeat [n, n-1..1])
    where 
    drop x n
        | n == 1 = []
        | otherwise = [x]

-- p17
myTake :: Int -> [a] -> [a]
myTake n (x:xs)
    | n > 0 = x : myTake (n-1) xs
    | otherwise = []

myDrop :: Int -> [a] -> [a]
myDrop n all@(x:xs)
    | n > 0 = myDrop (n-1) xs
    | otherwise = all

mySplit :: [a] -> Int -> [[a]]
mySplit xs n = [myTake n xs, myDrop n xs]

-- p18
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs min max = 
    [ snd x | x <- (zip [1..] xs), (fst x)>=min, (fst x)<=max ]

-- p19
myRotate :: [a] -> Int -> [a]
myRotate xs n 
    | n > 0 = snd (splitAt n xs)
    | n < 0 = (snd negPair) ++ (fst negPair)
    | otherwise = xs
    where negPair = (splitAt ((length xs)+n) (xs))

-- p20
myRemoveAt :: [a] -> Int -> (a, [a])
myRemoveAt xs n = (xs !! (n-1) , (fst pair) ++ tail (snd pair))
    where pair = splitAt (n-1) xs

-- p21
myInsertAt :: a -> [a] -> Int -> [a]
myInsertAt x ys n = (pair !! 0) ++ [x] ++ (pair !! 1)
    where pair = (mySplit ys (n-1))

-- p22
myRange :: Int -> Int -> [Int]
myRange x y 
    | x<=y = x : myRange (x+1) y
    | otherwise = []

-- p23
{--
myRandBool :: IO Bool -- m Bool?
myRandBool = do
    gen <- newStdGen
    let (r, g) = random gen :: (Bool, StdGen)
    return r
--}


myRandSelect :: [a] -> Int -> IO [a]
myRandSelect xs n = do
    randomList <- myRandomize xs
    return (take n randomList)

-- p24
myRandomSelect :: Int -> Int -> IO [Int]
myRandomSelect n r = do
    let list = [1..r]
    myRandSelect list n


-- p25
myRandomize :: [a] -> IO [a]
myRandomize [] = do 
    return []
myRandomize xs = do
    gen <- newStdGen
    let len = length xs
    let n = fst (randomR (1, len) gen :: (Int, StdGen))
    let s = myRemoveAt xs n
    rest <- (myRandomize (snd s))
    return ((fst s) : rest)

-- p26
-- myRest 3 [1..5] = [3..5]
myRest :: (Eq a) => a -> [a] -> [a]
myRest _ [] = []
myRest x (y:ys)
    | x == y = ys
    | otherwise = myRest x ys

myCombinations :: (Eq a) => Int -> [a] -> [[a]]
myCombinations n xs
    | n > 1 = concat ( map (\x -> [x:y | y <- (myCombinations (n-1) (myRest x xs))]) xs )
    | n == 1 = [[x] | x <- xs]
    | otherwise = []

-- p31
isprime :: Int -> Bool
isprime x
    | x <= 1 = False
    | divisable x [2..(x-1)] = False
    | otherwise = True
    where 
      divisable x (y:ys)
        | x `mod` y == 0 = True
        | otherwise = divisable x ys
      divisable x [] = False

-- p32
gcd' :: Int -> Int -> Int
gcd' x y 
    | y == 0 = x
    | otherwise = gcd' y (x `mod` y) -- TODO: negative numbers

-- p33
coprime :: Int -> Int -> Bool
coprime x y
    | gcd' x y == 1 = True
    | otherwise = False

-- p34
totient :: Int -> Int
totient x
    | x == 1 = 1
    | otherwise = length $ filter (coprime x) [1..x-1]

-- p35
primeFactor :: Int -> [Int]
primeFactor x
    | x /= 1 = firstPrimeFactor : primeFactor (x `div` firstPrimeFactor)
    | otherwise = []
    where 
    firstPrimeFactor = firstPrimeFactor' x (filter isprime [2..x-1])
        where 
        firstPrimeFactor' y [] = y
        firstPrimeFactor' y (z:zs)
            | mod y z == 0 = z
            | otherwise = firstPrimeFactor' y zs
        
