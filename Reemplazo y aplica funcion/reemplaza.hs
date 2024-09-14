-- Función recursiva que reemplaza los valores impares por su valor negativo
reemplazarImpares :: [Int] -> [Int]
reemplazarImpares [] = []  -- Caso base: lista vacía
reemplazarImpares (x:xs)
    | odd x     = (-x) : reemplazarImpares xs  
    | otherwise = x : reemplazarImpares xs     


main :: IO ()
main = do
    let lista = [2, 1, 3, 4, 5]
    print $ reemplazarImpares lista
