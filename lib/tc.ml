(** Type inference accepts a term and decorates the term with a type*)
    
type tcPayload = {position: Parser.position; ty: ty}
and kind = unit Common.kind
and ty = unit Common.ty 
and term = tcPayload Common.term 

let pp_print_unit: Format.formatter -> unit -> unit = fun _ _ -> ()

let pp_print_kind = Common.pp_print_kind pp_print_unit
let pp_print_ty = Common.pp_print_ty pp_print_unit
let pp_print_tcpl: Format.formatter -> tcPayload -> unit =
  fun ppf {position; ty} ->
    Format.fprintf ppf "{%a;@, %a}" Parser.pp_print_position position pp_print_ty ty
let pp_print_term = Common.pp_print_term pp_print_tcpl

module IdentOrd = struct
  type t = Common.ident
  let compare i1 i2 = Stdlib.compare i1 i2
end

module IMap = struct
  (** Include everything that Map gives us *)
  include Map.Make(IdentOrd)

  let pp_print_type_binding ppf = fun i ty -> 
    Format.fprintf ppf "(%a:%a),@, " Common.pp_print_ident i pp_print_ty ty
  
  (** Pretty print type context *)
  let pp_print_tymap ppf = iter (pp_print_type_binding ppf)   

  (** Pretty print the complete type checker context*)
  let pp_print_tcCtx ppf ctx
  = Format.fprintf ppf
      "TypeContext: {@ %a@ }"
      pp_print_tymap ctx
      
end

(** A typing context maps variables to types *)
type tcCtx = ty IMap.t 

type subst = ty IMap.t

type 'a tcResult = ('a, string) result 
    
let pp_print_tcCtx = IMap.pp_print_tcCtx

let typeCheck: tcCtx -> Parser.term -> term tcResult = fun _ -> Common.todo __LOC__                
let typeInfer: tcCtx -> Parser.term -> (ty * subst * term) tcResult = fun _ -> Common.todo __LOC__ 
