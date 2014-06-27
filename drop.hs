drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' num all@(x:xs) 
    | num > 0 = drop (num-1) xs
    | otherwise = all

