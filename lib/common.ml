(* let todo loc = function | _ -> failwith "Unimplemented feature error at: " ^ loc  *)

let todo loc = failwith ("Unsupported operation at " ^ loc)

type ident = string
and 'a kind =
  [ `KStar of 'a
  | `KArr of 'a * 'a kind * 'a kind
  | `KApp of 'a * 'a kind * 'a kind 
  ]
and 'a ty =
  [ `TVar of 'a * ident
  | `TBool of 'a
  | `TNat of 'a
  | `TArr of 'a * 'a ty * 'a ty
  ]    
and 'a term =
  [ `Var of 'a * ident
  | `Const of 'a * ident
  | `Lam of 'a * ident * 'a ty * 'a term
  | `App of 'a * 'a term * 'a term
  ] 

let pp_print_ident ppf i = Format.fprintf ppf "@[%s@]" i

let rec pp_print_kind: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a kind -> unit =
  fun pp ppf -> function
    | `KStar pl -> Format.fprintf ppf "@[*@]@, ("; pp ppf pl; Format.fprintf ppf ")" 
    | `KArr (pl, k1, k2) -> (pp_print_kind pp ppf k1)
                          ; Format.fprintf ppf "=>"
                          ; (pp_print_kind pp ppf k2)
                          ; pp ppf pl 
    | `KApp (pl, k1, k2) -> (pp_print_kind pp ppf k1)
                          ; (pp_print_kind pp ppf k2)
                          ; pp ppf pl 

let rec pp_print_ty: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a ty -> unit
  = fun pp ppf -> function
    | `TVar (pl, i) -> Format.fprintf ppf "@[%a@]" pp_print_ident i
                    ; pp ppf pl
    | `TBool pl -> Format.fprintf ppf "%s" "Bool"; pp ppf pl
    | `TNat pl -> Format.fprintf ppf "%s" "Nat"; pp ppf pl
    | `TArr (pl, ty1, ty2) -> (pp_print_ty pp ppf ty1)
                          ; Format.fprintf ppf "->@, "
                          ; pp ppf pl
                          ; (pp_print_ty pp ppf ty2)
                          

let rec pp_print_term: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a term -> unit =
  fun pp ppf -> function
    | `Var (pl , i) 
    | `Const (pl, i) -> Format.fprintf ppf "%a" pp_print_ident i
                     ; pp ppf pl
                     ; Format.fprintf ppf ""
    | `Lam (pl, i, ty, t) -> Format.fprintf ppf "λ%a:%a" 
                           pp_print_ident i (pp_print_ty pp) ty
                       ; pp ppf pl
                       ; Format.fprintf ppf ". @, %a"
                           (pp_print_term pp) t
    | `App (pl, t1, t2) -> Format.fprintf ppf "%a@, %a ["
                           (pp_print_term pp) t1
                           (pp_print_term pp) t2
                     ; pp ppf pl
                     ; Format.fprintf ppf "]"

type 'a program = 'a term list

let pp_print_prog: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a program -> unit =
  fun _ -> todo __LOC__

type 'a progParser = string -> 'a program 

type 'a termParser = string -> 'a term 
