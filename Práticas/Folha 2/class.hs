classifica :: Int -> String
classifica x = if x <= 9 then "Reprovado" else (if x >= 10 && x <= 12 then "Suficiente" else (if x >= 13 && x<= 15 then "Bom" else (if x >= 16 && x <= 18 then "Muito Bom" else "Muito Bom Com Distincao")))

classifica' :: Int -> String
classifica' x | x <= 9 = "Reprovado"
              | x >= 10 && x <= 12 = "Suficiente"
              | x >= 13 && x <= 15 = "Bom"
              | x >= 16 && x <= 18 = "Muito Bom"
              | otherwise = "Muito Bom Com Distincao"
