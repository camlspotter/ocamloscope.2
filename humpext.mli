(** Hump extraction. *)

val test : bool ref
(** Test mode permits unresolved persistent idents *)  

module Types : sig
  val signature : Env.t -> Types.signature -> Hump.expr
end
  
val signature : Hump.path -> Typedtree.signature -> Hump.expr
val structure : Hump.path -> Typedtree.structure -> Hump.expr
