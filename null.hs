null' :: [a] -> Bool
-- null' xs = if length xs == 0 then True else False
null' xs    
    | length xs == 0 = True
    | otherwise = False

