quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ larger
                   where smaller = quicksort [e | e <- xs, e <= x]
                         larger  = quicksort [e | e <- xs, e > x]
