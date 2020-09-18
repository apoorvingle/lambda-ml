(** Type inference accepts a term and decorates the term with a type*)
open Stdlib.Result
       
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

let lookup: tcCtx -> Common.ident -> ty =
  fun ctx i -> IMap.find i ctx

let add: tcCtx -> Common.ident -> ty -> tcCtx =
  fun ctx i ty -> IMap.add i ty ctx 

let typeError s = Error ("Type Error: " ^ s)
let unificationError = fun expTy infTy ->
  typeError ("Cannot unify expected type: " ^ Common.string_of_t pp_print_ty expTy
             ^ " with infered type: " ^ Common.string_of_t pp_print_ty infTy)
    
let typeCheckConst: Common.const -> ty -> Common.const tcResult =
  fun trm expTy -> match trm with
    | Common.NumC _ ->
      if expTy  = `TNat ()
      then ok trm
      else unificationError expTy (`TNat ()) 
    | Common.BoolC _ ->
      if expTy  = `TBool ()
      then ok trm
      else unificationError expTy (`TBool ()) 
  

let checkType: tcCtx -> Parser.term -> ty -> term tcResult =
  fun ctx trm expTy -> match trm with
    | `Var (pos, i) ->
      let infTy = lookup ctx i in 
      if (expTy = infTy)
      then ok (`Var ({position=pos; ty=expTy}, i))
      else (unificationError expTy infTy) 
    | `Const (pos, c) -> bind (typeCheckConst c expTy) (fun c ->
        ok (`Const ({position=pos; ty=expTy}, c)))
    | `App _
    | `Lam _ -> Common.todo __LOC__
    (* | `Lam (pos, i, argTy, body) -> bind (inferType (add ctx i argTy) body) (fun (ty, _, trm') ->
     *     if expTy = (`TArr () argTy ty)
     *     then ok (`Lam ({position=pos, expTy}, i, argTy, trm'))
     *     else unificationError expTy (`TArr () argTy ty)) *)
                  
and inferType: tcCtx -> Parser.term -> (ty * subst * term) tcResult = fun _ -> Common.todo __LOC__ 
