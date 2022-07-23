metades :: [a] -> ([a], [a])
metades l = (take (div (length l) 2) l, drop (div (length l) 2) l)
