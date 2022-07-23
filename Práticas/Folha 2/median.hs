mediana :: Ord a => a -> a -> a -> a
mediana x y z | x >= y && x <= z = x
              | x <= y && x >= z = x
              | y >= x && y <= z = y
              | y <= x && y >= z = y
              | otherwise = z

mediana' :: (Num a, Ord a) => a -> a -> a -> a
mediana' x y z = x1 - x2 - x3
      where x1 = x + y + z
            x2 = max (max x y) z
            x3 = min (min x y) z
