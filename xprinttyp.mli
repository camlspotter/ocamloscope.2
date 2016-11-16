(** Modified version of Printtyp of OCaml compiler *)

open Types
open Outcometree

module Make(A : sig val rewrite : Path.t -> Outcometree.out_ident end) : sig

  (** Converters from OCaml types/paths to Outcometree,
      with capability of rewriting paths on the way *)

  val tree_of_type_scheme: type_expr -> out_type
  val tree_of_path : Path.t -> out_ident (* XXX This is actually A.write *)

  val string_of_out_ident : out_ident -> string
end
