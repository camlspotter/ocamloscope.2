module Make(A : sig
  val global_source : Hump.path -> Hump.expr option
  (** loader of persistent ident of module *)

  val go_on_even_at_coercion_errors : bool
  (** Flag not to abort at evaluation errors of module coercions *)    
end) : sig
  val eval_global : Hump.path -> Hump.expr option
  val eval : (Hump.id * Hump.expr option) list -> Hump.expr -> Hump.expr
end
