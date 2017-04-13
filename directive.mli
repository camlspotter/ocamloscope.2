open Spotlib.Spot

val parse : string -> (Parsetree.expression, [> `Exn of exn]) Result.t option
(** Parse directives. It starts with '#', then followed by an OCaml expression *)

val interpret
  : Data.DB.t
    -> Parsetree.expression
    -> Conf.t
    -> Query.PackageSpec.t
    -> Conf.t * Query.PackageSpec.t
(** Execute an directive. Directive execution may modify the package spec.

    Supported directives are:

    #packages : List the packages linked
    #all      : Set the search targets to all the packages
    #vanilla  : Set the search targets to the OCaml standard (-ocamldoc -compilerlibs)
    #none     : Set the search targets to the empty set
    #quit     : Quit the search session
 *)
