(** The parser needs to have a parse function that parses the input string
 * and then stores additional information such as location position *)

type position = {fname: string; lno: int; cno: int}

val dummyPos: position
  
val pp_print_position: Format.formatter -> position -> unit

(* A parser kind *)
type kind = position Common.kind
(* parser type *)
type ty = position Common.ty 
(* parser term *)
type term =  position Common.term

(* Parse a term *)
val parseTerm: string -> term  

(** Pretty printers for parser outputed kind, type and term *)
val pp_print_kind: Format.formatter -> kind -> unit
val pp_print_ty: Format.formatter -> ty -> unit
val pp_print_term: Format.formatter -> term -> unit
