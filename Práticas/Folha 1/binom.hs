binom :: Integer -> Integer -> Integer
binom n k = div x1 x2
 where x1 = product [1..n]
       x2 = product [1..k] * product [1..x3]
       x3 = n-k

binom' :: Integer -> Integer -> Integer
binom' n k = if k < x4 then div x5 x6 else div x7 x8
  where x4 = n-k
        x5 = product[(x4+1)..n]
        x6 = product [1..k]
        x7 = product[(k+1)..n]
        x8 = product[1..(n-k)]
