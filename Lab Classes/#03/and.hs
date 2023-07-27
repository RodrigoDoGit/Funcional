myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x == True && myand xs
