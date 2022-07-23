unidades :: [String]
unidades =
  [ "zero"
  , "um"
  , "dois"
  , "tres"
  , "quatro"
  , "cinco"
  , "seis"
  , "sete"
  , "oito"
  , "nove"
  ]

dez_a_dezanove :: [String]
dez_a_dezanove =
  [ "dez"
  , "onze"
  , "doze"
  , "treze"
  , "catorze"
  , "quinze"
  , "dezasseis"
  , "dezassete"
  , "dezoito"
  , "dezanove"
  ]

dezenas :: [String]
dezenas =
  [ "vinte"
  , "trinta"
  , "quarenta"
  , "cinquenta"
  , "sessenta"
  , "setenta"
  , "oitenta"
  , "noventa"
  ]

converte2 :: Int -> String
converte2 n | n < 100 = combina2 (divide2 n)

divide2 :: Int -> (Int, Int)
divide2 n = (div n 10, mod n 10)

combina2 :: (Int, Int) -> String
combina2 (0, u) = unidades !! u
combina2 (1, u) = dez_a_dezanove !! u
combina2 (d, 0) = dezenas !! (d-2)
combina2 (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

centenas :: [String]
centenas =
  [ "cento"
  , "duzentos"
  , "trezentos"
  , "quatrocentos"
  , "quinhentos"
  , "seiscentos"
  , "setecentos"
  , "oitocentos"
  , "novecentos"
  ]

converte3 :: Int -> String
converte3 n | n < 1000 = combina3 (divide3 n)

divide3 :: Int -> (Int, Int)
divide3 n = (div n 100, mod n 100)

combina3 :: (Int, Int) -> String
combina3 (0, n) = converte2 n
combina3 (1, 0) = "cem"
combina3 (c, 0) = centenas !! (c-1)
combina3 (c, n) = centenas !! (c-1) ++ " e " ++ converte2 n

converte6 :: Int -> String
converte6 n | n < 1000000 = combina6 (divide6 n)

divide6 :: Int -> (Int, Int)
divide6 n = (div n 1000, mod n 1000)

combina6 :: (Int, Int) -> String
combina6 (0, n) = converte3 n
combina6 (1, 0) = "mil"
combina6 (1, n) = "mil" ++ ligar n ++ converte3 n
combina6 (m, 0) = converte3 m ++ "mil"
combina6 (m, n) = converte3 m ++ " mil " ++ ligar n ++ converte3 n

ligar :: Int -> String
ligar r | r < 100 || mod r 100 == 0 = " e "
        | otherwise = " "

converte :: Int -> String
converte n = converte6 n
