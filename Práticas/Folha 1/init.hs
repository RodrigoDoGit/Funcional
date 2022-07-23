myinit :: [a] -> [a]
myinit l = reverse(tail (reverse l))

myinit' :: [a] -> [a]
myinit' l = reverse(drop 1 (reverse l)) 
