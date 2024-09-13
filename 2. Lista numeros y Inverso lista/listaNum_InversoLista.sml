(* Función que devuelve una lista de números de 0 a n *)
fun listUpTo n =
    List.tabulate (n + 1, fn i => i);

(*Funcion para invertir lista*)
fun reverseList lst =
    List.rev lst;
