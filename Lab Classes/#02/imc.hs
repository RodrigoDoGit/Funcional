classifica :: Float -> Float -> String
classifica x y | imc < 18.5 = "Baixo Peso"
               | imc >= 18.5 && imc < 25 = "Peso Normal"
               | imc >= 25 && imc < 30 = "Excesso de Peso"
               | otherwise = "Obesidade"
     where imc = x/(y*y)
