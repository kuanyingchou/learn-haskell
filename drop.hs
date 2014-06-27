drop' :: Int -> [a] -> [a]
drop' num all@(x:xs) 
    | num > 0 = drop (num-1) xs
    | otherwise = all

