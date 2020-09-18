(** Type inference accepts a term and decorates the term with a type*)
type tcPayload = {position: Parser.position; ty: ty}
and kind = unit Common.kind
and ty = unit Common.ty 
and term = tcPayload Common.term 
type tcCtx
type subst

type 'a tcResult = ('a, string) result 

val pp_print_tcpl: Format.formatter -> tcPayload -> unit
val pp_print_tcCtx: Format.formatter -> tcCtx -> unit

val pp_print_kind: Format.formatter -> kind -> unit
val pp_print_ty: Format.formatter -> ty -> unit
val pp_print_term: Format.formatter -> term -> unit


val typeCheck: tcCtx -> Parser.term -> ty -> term tcResult 
val typeInfer: tcCtx -> Parser.term -> (ty * subst * term) tcResult
