import System.IO (hFlush, stdout)

data Animal = Animal
    { tipo   :: String
    , apodo  :: String
    } deriving (Show)

type ListaAnimales = [Animal]

-- Función que muestra el menú y procesa la entrada del usuario
menuPrincipal :: ListaAnimales -> IO ()
menuPrincipal lista = do
    putStrLn "Opciones del menú:"
    putStrLn "1. Añadir animal"
    putStrLn "2. Ver todos"
    putStrLn "3. Salir"
    putStr "Elija una opción: "
    hFlush stdout  
    eleccion <- getLine
    case eleccion of
        "1" -> do
            nuevaLista <- añadirAnimal lista
            menuPrincipal nuevaLista
        "2" -> do
            mostrarLista lista
            menuPrincipal lista
        "3" -> putStrLn "¡Adiós!"
        _   -> do
            putStrLn "Opción incorrecta. Intente de nuevo."
            menuPrincipal lista

-- Función para agregar un nuevo animal
añadirAnimal :: ListaAnimales -> IO ListaAnimales
añadirAnimal lista = do
    putStr "Ingrese el tipo de animal: "
    hFlush stdout
    tipoAnimal <- getLine
    putStr "Ingrese el apodo del animal: "
    hFlush stdout
    apodoAnimal <- getLine
    let nuevoAnimal = Animal tipoAnimal apodoAnimal
    return (nuevoAnimal : lista)

-- Función para mostrar todos los animales
mostrarLista :: ListaAnimales -> IO ()
mostrarLista [] = putStrLn "No hay animales registrados."
mostrarLista (y:ys) = do
    putStrLn $ "Tipo: " ++ tipo y ++ ", Apodo: " ++ apodo y
    mostrarLista ys

-- Función principal
main :: IO ()
main = menuPrincipal []
