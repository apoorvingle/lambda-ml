open Common

(** The parser needs to have a parse function that parses the input string
 * and then stores additional information such as location position *)

type position = {fname: string; lno: int; cno: int}

let dummyPos = {fname =  ""; lno = 0; cno = 0}
               
let pp_print_position: Format.formatter -> position -> unit =
  fun ppf {fname; lno; cno} -> Format.fprintf ppf "(%s:%d:%d)" fname lno cno

(* A parser kind *)
type kind = position Common.kind
(* parser type *)
type ty = position Common.ty 
(* parser term *)
type term =  position Common.term


let pp_print_kind =  Common.pp_print_kind pp_print_position
let pp_print_ty = Common.pp_print_ty pp_print_position
let pp_print_term = Common.pp_print_term pp_print_position
  
let parseTerm = fun _ -> todo __LOC__
