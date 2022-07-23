curta :: [a] -> Bool
curta l = length l >= 0 && length l <= 2

curta' :: [a] -> Bool
curta' [] = True
curta' [x] = True
curta' (x:xs) = length xs < 2
