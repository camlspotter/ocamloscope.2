(** conversions from Parsetree idents and types to Outcometree *)
  
open Outcometree

val out_of_longident : Longident.t -> out_ident

val out_of_core_type : Parsetree.core_type -> out_type
(* Not well tested. TODO: check *)
