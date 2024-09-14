-- Función recursiva que cuenta el número de comas en un string
contarComas :: String -> Int
contarComas [] = 0  -- Caso base: cadena vacía, no hay comas
contarComas (x:xs)
    | x == ','  = 1 + contarComas xs  
    | otherwise = contarComas xs       

main :: IO ()
main = do
    let texto = "Hola, mundo, cómo, estás?"
    print $ contarComas texto
