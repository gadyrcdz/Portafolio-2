

(*****FACTORIAL*********)

fun factorial 1 = 1
    | factorial n = n * factorial(n-1);


fun printFromZero1 () =
    let
        fun loop n =
            if n > 10 then ()
            else (print (Int.toString n ^ "\n"); loop (n + 1))
    in
        loop 0
    end;



(* Función auxiliar para determinar si un número es primo *)
fun isPrime n =
    let
        fun checkDivisor d =
            d * d > n orelse (n mod d <> 0 andalso checkDivisor (d + 1))
    in
        if n < 2 then false
        else checkDivisor 2
    end;

(* Función para imprimir los números primos desde 0 hasta 1000 *)
fun printPrimesFromZero () =
    let
        (* Generar una lista de números desde 0 hasta 1000 *)
        val numbers = List.tabulate (1001, fn i => i)
        (* Filtrar los números primos *)
        val primes = List.filter isPrime numbers
    in
        (* Imprimir solo los números primos *)
        List.app (fn x => print (Int.toString x ^ "\n")) primes
    end;

