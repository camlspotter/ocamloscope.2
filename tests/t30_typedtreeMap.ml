(* functor whose parameter names are different from sig and str *)

(*
  ((KValue,
    "NOTOP.T30_typedtreeMap.MakeMap(Map \"NOTOP.T30_typedtreeMap\" 1211).map_module_type"),
   Coerced
     (Def
        ("NOTOP.T30_typedtreeMap.MakeMap(Map).map_module_type",
         "t30_typedtreeMap.ml:38,6--21"),
      Def
        ("NOTOP.T30_typedtreeMap.MakeMap.map_module_type",       <======= bug!  it must be MakeMap(Iter).map_module_type
         "t30_typedtreeMap.ml:29,6--21"))) ]
*)

type structure
type pattern
type structure_item
type expression
type class_expr
type signature
type signature_item
type module_type
  
module type MapArgument = sig
  val enter_structure : structure -> structure
  val leave_structure : structure -> structure
end

module MakeMap :
  functor
    (Iter : MapArgument) ->
sig
  val map_structure : structure -> structure
  val map_pattern : pattern -> pattern
  val map_structure_item : structure_item -> structure_item
  val map_expression : expression -> expression
  val map_class_expr : class_expr -> class_expr

  val map_signature : signature -> signature
  val map_signature_item : signature_item -> signature_item
  val map_module_type : module_type -> module_type
end = functor (Map : MapArgument) -> struct
  let map_structure : structure -> structure = assert false
  and map_pattern : pattern -> pattern = assert false
  and map_structure_item : structure_item -> structure_item = assert false
  and map_expression : expression -> expression = assert false
  and map_class_expr : class_expr -> class_expr = assert false
  and map_signature : signature -> signature = assert false
  and map_signature_item : signature_item -> signature_item = assert false
  and map_module_type : module_type -> module_type = assert false
end
  
