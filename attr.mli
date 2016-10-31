(*   Only works with 4.03.0. Need to carefully check when porting to 
   newer OCaml releases.
*)

open Typedtree
  
(** Iterator over the OCaml attributes *)
module Make(A : sig val f : attributes -> unit end) : sig
  val iter_structure      : structure -> unit
  val iter_signature      : signature -> unit
  val iter_structure_item : structure_item -> unit
  val iter_signature_item : signature_item -> unit
  val iter_expression     : expression -> unit
  val iter_module_type    : module_type -> unit
  val iter_pattern        : pattern -> unit
  val iter_class_expr     : class_expr -> unit
end
