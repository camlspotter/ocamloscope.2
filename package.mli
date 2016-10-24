open Spotlib.Spot
open Utils
open Opamfind

val stdlib_dir : FilePath.t Lazy.t

val sw : Opam.Switch.t Lazy.t
  
val packages : Ocamlfind.Package.t list Lazy.t
(** OCamlFind packages found in the system *)

val opam_packages : Opamfind.Opam.Package.t list Lazy.t
(** OPAM packages found in the system *)
  
val ocamlfind_package_groups : Ocamlfind.Analyzed_group.t list Lazy.t
(** OCamlFind package groups found in the system *)
  
val ocamlfinds_of_opam : (Opam.Package.t * Ocamlfind.Analyzed_group.t list) list Lazy.t
(** OCamlFind packages provided by each OPAM package *)
  
val opams_of_ocamlfind : (Ocamlfind.Analyzed_group.t * Opam.Package.t list) list Lazy.t
(** The OPAM package which provides each OCamlFind package group *)
