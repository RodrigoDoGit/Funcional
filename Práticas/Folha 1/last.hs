mylast :: [a] -> a
mylast l = head (reverse l)

mylast' :: [a] -> a
mylast' l = head (reverse(drop (length l-1) l))
