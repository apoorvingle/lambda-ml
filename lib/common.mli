val todo: string -> 'a
  
type ident = string

type 'a kind =
  [ `KStar of 'a
  | `KArr of 'a * 'a kind * 'a kind
  | `KApp of 'a * 'a kind * 'a kind 
  ]

val pp_print_kind: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a kind -> unit

type 'a ty =
  [ `TVar of 'a * ident
  | `TBool of 'a
  | `TNat of 'a
  | `TArr of 'a * 'a ty * 'a ty
  ]

val pp_print_ty: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a ty -> unit

type const =
  | NumC of int
  | BoolC of bool

type 'a term =
  [ `Var of 'a * ident
  | `Const of 'a * const
  | `Lam of 'a * ident * 'a ty * 'a term
  | `App of 'a * 'a term * 'a term
  ] 

val pp_print_ident: Format.formatter -> ident -> unit

val pp_print_const: const -> unit
val pp_print_term: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a term -> unit

type 'a program = 'a term list

val pp_print_prog: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a program -> unit

type 'a progParser = string -> 'a program 

type 'a termParser = string -> 'a term 

val string_of_t: (Format.formatter -> 'a -> unit) -> 'a -> string
