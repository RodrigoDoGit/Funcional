min3 :: Ord a => a -> a -> a -> a
min3 x y z | x <= y && x <= z = x
           | y <= x && y <= z = y
           | otherwise = z

min3' :: Ord a => a -> a -> a -> a
min3' x y z = min (min x y) z
