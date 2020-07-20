open Lib

(** λi:'a -> i*)
let idLam: Parser.term =  `Lam (Parser.dummyPos, "i", `TVar (Parser.dummyPos, "'a")
                                , (`Var (Parser.dummyPos, "i"))) 

let _ =
  print_endline "Hello world!"
; print_endline (string_of_int (Ast.add 3 4))
; Format.printf "%a" (Common.pp_print_term Parser.pp_print_position) idLam
