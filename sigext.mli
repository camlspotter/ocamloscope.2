(** Extraction of Sigs: names and types from *.cmi files *)

open Types
open Sig

module Print : sig

  val path_simplifier
    : k
    -> out_ident
    -> out_ident
    -> out_ident

  val fsignature_item
    : bool (** recursively print the internal *)
    -> Format.formatter -> fsignature_item -> unit
  (** print [fsignature_item] in human friendly form *)

  module Make(A : sig
    val path_simplifier : out_ident -> out_ident
  end) : sig
    val simplif_type : out_type -> out_type
    val string_of_ident : out_ident -> string
    val string_of_type : out_type -> string
    val fmodule : bool -> fmodule -> Outcometree.out_module_type
    val fsignature : bool -> fsignature -> Outcometree.out_sig_item list
    val rec_status : rec_status -> Outcometree.out_rec_status
    val ftypekind : ftypekind -> out_type
    val fsignature_item : bool -> fsignature_item -> Outcometree.out_sig_item option
  end
end

val scrape : Path.t option -> signature -> fsignature
(** [scrape p sg] converts the signature [sg] of a module of 
    a global access path [p] to [fsignature]. 

    Usually [p] must be [Some _], [p = None] only when scraping
    predefined values and types.
*)

val test : Path.t option -> signature -> unit
