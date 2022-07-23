import Data.Char (chr, ord)

letraInt :: Char -> Int
letraInt c = ord c - ord 'A'

intLetra :: Int -> Char
intLetra n = chr (n + ord 'A')

maiúscula :: Char -> Bool
maiúscula x = x >= 'A' && x <= 'Z'

desloca :: Int -> Char -> Char
desloca k x | maiúscula x = intLetra (mod (letraInt x + k) 26)
            | otherwise = x

cifrar :: Int -> String -> String
cifrar k xs = [desloca k x | x <- xs]

tabelaPT :: [Float]
tabelaPT = [13.9, 1.0, 4.4, 5.4, 12.2, 1.0,
            1.2, 0.8, 6.9, 0.4, 0.1, 2.8, 4.2,
            5.3, 10.8, 2.9, 0.9, 6.9, 7.9, 4.9,
            4.0, 1.3, 0.01, 0.3, 0.01, 0.4]

ocorrências :: Char -> String -> Int
ocorrências y xs = length [x | x <- xs, x == y]

contarMaiúsculas :: String -> Int
contarMaiúsculas xs = length [x | x <- xs, maiúscula x]

porcento :: Int -> Int -> Float
porcento num denom = (fromIntegral num / fromIntegral denom) * 100

freqs :: String -> [Float]
freqs xs = [porcento (ocorrências x xs) n | x <- ['A'..'Z']]
 where n = contarMaiúsculas xs

chiquad :: [Float] -> [Float] -> Float
chiquad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rodar :: Int -> [a] -> [a]
rodar n xs = drop n xs ++ take n xs

indices :: Eq a => a -> [a] -> [Int]
indices x ys = [i | (i,y) <- zip [0..n] ys, x == y]
     where n = length ys + 1

quebrar :: String -> Int
quebrar xs = k
     where
       obs = freqs xs
       chitab = [chiquad (rodar k obs) tabelaPT | k <- [0..25]]
       k = head (indices (minimum chitab) chitab)
