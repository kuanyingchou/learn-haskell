-- 2014.6.18  ken  initial version

center :: Int -> String -> String
center n str = ws ++ str ++ ws
    where wsLen = (n - length str) `div` 2 
          ws = replicate wsLen ' ' 

mountain :: Int -> Int -> [Int]
mountain max step = [1, 1+step..max] ++ [max-step, max-2*step..1]

diamond n char = if even n then error "n should be odd" 
    else [ center n (replicate x char) | x <- (mountain n 2) ]

printDiamond :: [String] -> IO ()
printDiamond xs = mapM_ putStrLn xs

fill :: [String] -> Int -> Int -> Char -> Char -> [String]
fill img x y from to 
    | x < 0 || x >= (length (img !! 0)) = img
    | y < 0 || y >= (length img) = img
    | img !! y !! x /= from = img
    | otherwise = fill (fill (fill (fill newImg (x-1) y from to) (x+1) y from to) x (y-1) from to) x (y+1) from to
    where (upperHalf, targetLine:lowerHalf) = splitAt y img
          (left, targetChar:right) = splitAt x targetLine
          newImg = upperHalf ++ [left ++ (to: right)] ++ lowerHalf


{--
convert :: Char -> Char -> Char -> Char
convert c f t = if c == f then t else c 

replace :: [String] -> Char -> Char -> [String]
replace img from to = [ [ convert pixel from to | pixel <- line] | line <- img ]

--}
{--
-- star :: Int -> String
star x = replicate x '*'
         --['*' | n <- [1..x] ]


-- incStar :: Int -> [String]
incStar n = [star x | x <- [1, 3..n]]

-- decStar :: Int -> [String]
decStar n = [star x | x <- [n, n-2..1]]


-- diamond :: Int -> [String]
diamond n = map (center n) (incStar n ++ decStar (n-2))
--}

{--
fillSingle :: [String] -> Int -> Int -> Char -> Char -> [String]
fillSingle img x y from to = upperHalf ++ [left ++ ((convert c from to) : right)] ++ lowerHalf
    where (upperHalf, currentLine:lowerHalf) = splitAt y img
          (left, c:right) = splitAt x currentLine
--}
