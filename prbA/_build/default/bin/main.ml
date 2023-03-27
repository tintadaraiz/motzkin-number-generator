(********************************************************************************************************************************************)
(* trabalho realizado por:                                                                                                                  *)
(* - a48069: Rodrigo Silva                                                                                                                  *)
(* - a48990: Leonardo Santos                                                                                                                *)
(********************************************************************************************************************************************)

(********************************************************************************************************************************************)
(*      fontes                                                                                                                              *)
(* https://en.wikipedia.org/wiki/Motzkin_number                                                                                             *)
(* https://ocaml.org/docs/hash-tables                                                                                                       *)
(* https://stackoverflow.com/questions/14454981/memoization-in-ocaml                                                                        *)
(* https://probablydance.com/2018/06/16/fibonacci-hashing-the-optimization-that-the-world-forgot-or-a-better-alternative-to-integer-modulo/ *)
(* https://cs3110.github.io/textbook/chapters/ds/memoization.html                                                                           *)
(* https://www.cs.mcgill.ca/~abeaul10/blog/2.html   <- última inspiração                                                                    *)
(********************************************************************************************************************************************)


(* programa *)
(* input (utilizador) *)
let num = read_int ()

(* criação da HashTable *)
let memo = Hashtbl.create (num+1)
let () = Hashtbl.add memo 0 Z.one
let () = Hashtbl.add memo 1 Z.one

(* memoization *)
let rec memoized_mot num =
  if (num >= 0 && num <= 10000) then
      match Hashtbl.mem memo num with
      | true -> Hashtbl.find memo num
      | false ->
        let op1 num = (2 * num) + 1  in (* funções auxiliares *)
        let op2 num = (3 * num) - 3 in
        let op3 num = num + 2 in
        let result = Z.div (Z.add (Z.mul (Z.of_int (op1 num)) (memoized_mot (num-1))) (Z.mul (Z.of_int (op2 num)) (memoized_mot (num-2)))) (Z.of_int (op3 num)) in
        Hashtbl.add memo num result;
        result
    else invalid_arg " o número introduzido tem de estar no intervalo 0 <= num <= 10000 "


(* num -> memoized_mot -> Z.print -> endline *)
let () = num |> memoized_mot |> Z.print; print_endline ("")


(********************************************************************************************************************************************)
(* exemplo de execução                                                                                                                  *)
(* - com dune                                                                                                                               *)
(* abra o Terminal (ou alternativa semelhante)                                                                                              *)
(* navegue para o diretório que contém o projeto                                                                                            *)
(* execute o comando dune build                                                                                                             *)
(* caso não tenha tido erros, execute o comando dune exec prbA                                                                              *)
(* [ambiente de Terminal (exemplo)]                                                                                                         *)
(* user@userpc prbA $ Insira um número inteiro:                                                                                             *)
(* 25                                                                                                                                       *)
(********************************************************************************************************************************************)
