-- Función que toma una función y una lista, y aplica la función a cada elemento de la lista
transformarLista :: (a -> b) -> [a] -> [b]
transformarLista _ [] = []  
transformarLista f (y:ys) = f y : transformarLista f ys

-- Función principal que ejecuta el ejemplo
main :: IO ()
main = do
    let numeros = [1, 2, 3, 4, 5]
    let duplicar = \x -> x * 2  -- Función que duplica cada número
    print $ transformarLista duplicar numeros
