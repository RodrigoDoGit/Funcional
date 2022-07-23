divprop :: Integer -> [Integer]
divprop n = [m | m <- [1..(n-1)], mod n m == 0]

primo :: Integer -> Bool
primo n | length (divprop n) > 1 = False
        | otherwise = True
