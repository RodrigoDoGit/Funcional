divprop :: Integer -> [Integer]
divprop n = [m | m <- [1..(n-1)], mod n m == 0]

perfeitos :: Integer -> [Integer]
perfeitos n = [m | m <- [1..(n-1)], m == sum(divprop m)]
