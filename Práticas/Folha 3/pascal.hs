pascal :: Integer -> [[Integer]]
pascal 0 = [[1]]
pascal 1 = [[1], [1,1]]
pascal n = let pList = pascal(n-1)
           in pList ++ [([1] ++ binom (last pList) ++ [1])]
  where binom (x:y:ys) = (x+y) : binom (y:ys)
        binom (x:[])   = []
