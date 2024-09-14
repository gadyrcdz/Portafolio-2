(* Función auxiliar para contar apariciones *)
fun contarApariciones _ [] = 0
  | contarApariciones term (y::ys) =
        (if term = y then 1 else 0) + contarApariciones term ys;

(* Función principal que solicita el archivo y el término, luego cuenta las veces que aparece *)
fun contarPalabra () =
    let
        val _ = print "Por favor, ingrese la ruta del archivo: ";
        val rutaOption = TextIO.inputLine TextIO.stdIn;
        val _ = print "Por favor, ingrese la palabra a contar: ";
        val palabraOption = TextIO.inputLine TextIO.stdIn;

        val ruta = case rutaOption of
                      SOME r => String.substring (r, 0, String.size r - 1)
                    | NONE => "";

        val palabra = case palabraOption of
                      SOME p => String.substring (p, 0, String.size p - 1)
                    | NONE => "";

        val contenido =
            case TextIO.openIn ruta of
                archivo =>
                    let
                        val datos = TextIO.inputAll archivo
                    in
                        TextIO.closeIn archivo;
                        datos
                    end;

        val listaPalabras = String.tokens (fn c => Char.isSpace c orelse Char.isPunct c) contenido;

        val total = contarApariciones palabra listaPalabras
    in
        print ("La palabra '" ^ palabra ^ "' aparece " ^ Int.toString total ^ " veces.\n")
    end;
