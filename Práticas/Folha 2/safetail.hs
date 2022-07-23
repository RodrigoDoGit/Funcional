safetail :: Eq a => [a] -> [a]
safetail l = if l == [] then [] else tail l

safetail' :: Eq a => [a] -> [a]
safetail' l | l == [] = []
            | otherwise = tail l

safetail'' :: Eq a => [a] -> [a]
safetail'' [] = []
safetail'' l = tail l
