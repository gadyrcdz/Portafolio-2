-- Función principal para identificar el valor más frecuente en la lista
valorMasFrecuente :: (Eq a, Ord a) => [a] -> a
valorMasFrecuente xs = mayorFrecuencia (calcularFrecuencias xs)

-- Función que calcula cuántas veces aparece cada elemento en la lista
calcularFrecuencias :: (Eq a) => [a] -> [(a, Int)]
calcularFrecuencias [] = []
calcularFrecuencias (y:ys) = agregarFrecuencia y ys (calcularFrecuencias ys)
  where
    agregarFrecuencia y ys frecs = (y, contarOcurrencias y ys) : filtrar y frecs
    contarOcurrencias y = length . filter (== y)
    filtrar _ [] = []
    filtrar y ((z, n):zs)
        | y == z    = filtrar y zs
        | otherwise = (z, n) : filtrar y zs

-- Función que encuentra el elemento con la mayor frecuencia
mayorFrecuencia :: (Ord a) => [(a, Int)] -> a
mayorFrecuencia [] = error "La lista está vacía"
mayorFrecuencia [(y, _)] = y
mayorFrecuencia ((y1, f1):(y2, f2):ys)
    | f1 >= f2  = mayorFrecuencia ((y1, f1) : ys)
    | otherwise = mayorFrecuencia ((y2, f2) : ys)

-- Función principal que ejecuta el programa
main :: IO ()
main = do
    let numeros = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5]
    print $ valorMasFrecuente numeros


