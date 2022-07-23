myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x == True || myor xs
