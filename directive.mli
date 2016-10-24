open Spotlib.Spot

val parse : string -> (Parsetree.expression, [> `Exn of exn]) Result.t option
(** Parse directives. It starts with '#', then followed by an OCaml expression *)

val interpret
  : Data.DB.t
  -> Parsetree.expression
  -> Query.PackageSpec.t
  -> Query.PackageSpec.t
(** Execute an directive. Directive execution may modify the package spec *)
