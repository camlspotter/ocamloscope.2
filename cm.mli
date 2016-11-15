(** Analyzer of *.cm* files: find out which OCamlFind package it belongs to *)

open Spotlib.Spot
open Utils
open Opamfind

val test_mode : bool ref
(** When set [true], [guess p] does not try the complex analysis of [p],
    but simply returns the default *)

type t = {
  paths      : Hump.path list; (** Global access paths of the file. One cm file may be accessible by more than one way. The heuristic best path comes at the head *)
  digest     : Digest.t; (** digest of cmi *)
  cmi        : FilePath.t; (** cmi file location. This must always exist. *)
  cmt        : FilePath.t option; (** cmt file location *)
  cmti       : FilePath.t option; (** cmti file location *)
  ocamlfinds : (Ocamlfind.Analyzed_group.t * Ocamlfind.Analyzed.t) list;
  opam       : Opam.Package.t option
}
[@@deriving conv{ocaml_of}]
(** The result type *)

val format : Format.t -> t -> unit

val non_opam_dirs : FilePath.t list ref
(** Package source directories which are out of OPAM build dirs. 

    Used for the functions in this module.
*)

val traverse_packages
  : Ocamlfind.Analyzed_group.t
  -> t list
(** Traverse modules accessible from the given OCamlFind package group 
    and return the list of them *)

val package_stamp : t list -> Digest.t list
(** Sorted digest of the traversed modules.  Used to determine whether
    packages are re-scraped. *)

val get_packed : Cmt_format.cmt_infos -> FilePath.t list
(** [get_packed cmt_infos] returns the packed module cmi files 
    if [cmt_infos] is of a packed module. *)

val guess : FilePath.t -> t list
(** Guess the global accessibility of the given OCaml source/object/interface 
    file.
    
    If [test_mode] is set to [true], [guess p] does not try complex analysis of [p].
    It just returns the default value.
 *)

val reset_cache : unit -> unit
(** Resets the caches used in this module *)

val test : string list (*+ ex. ["stdlib"] *) -> unit
(** Prints out the global accessibilities of the modules of the given OCamlFind packages *)
