

(* Función para leer un archivo CSV y devolver una lista de palabras *)
fun readCSV fileName =
    let
        (* Función para leer el archivo línea por línea *)
        fun readLines chan =
            case TextIO.inputLine chan of
                SOME line => line :: readLines chan
              | NONE => []
    in
        (* Abre el archivo y lee las líneas *)
        let
            val chan = TextIO.openIn fileName
            val lines = readLines chan
            val _ = TextIO.closeIn chan
        in
            (* Asumimos que el CSV tiene un campo por línea *)
            List.map (fn line => String.strip (line)) lines
        end
    end

(* Función para contar palabras que se repiten en una lista de palabras *)
fun countWords lst =
    let
        (* Función auxiliar para actualizar la lista de pares (palabra, frecuencia) *)
        fun updateCounts [] counts = counts
          | updateCounts (word::restWords) counts =
            let
                (* Busca si la palabra ya está en la lista de conteos *)
                val (typesAlreadyCounted, typesNotCounted) = List.partition (fn (w, _) => w = word) counts
                (* Determina el nuevo conteo *)
                val newCount = case typesNotCounted of
                    [] => 1
                  | (_, count):: _ => count + 1
                (* Actualiza la lista de conteos *)
                val updatedCounts = (word, newCount) :: typesAlreadyCounted
            in
                updateCounts restWords updatedCounts
            end
    in
        (* Filtra las palabras que aparecen más de una vez *)
        List.filter (fn (_, count) => count > 1) (updateCounts lst [])
    end

(* Función principal que coordina la lectura del archivo y el conteo de palabras *)
fun processCSV fileName =
    let
        (* Lee el archivo CSV *)
        val words = readCSV fileName
        (* Cuenta las palabras que se repiten *)
        val wordCounts = countWords words
    in
        (* Imprime el resultado *)
        List.app (fn (word, count) =>
            print (word ^ " aparece " ^ Int.toString count ^ " veces\n")) wordCounts
    end
