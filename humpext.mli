(** Hump extraction. *)

val test : bool ref
(** Test mode permits unresolved persistent idents *)  

val types_signature : Env.t -> Types.signature -> Hump.expr
val signature : Hump.path -> Typedtree.signature -> Hump.expr
val structure : Hump.path -> Typedtree.structure -> Hump.expr
