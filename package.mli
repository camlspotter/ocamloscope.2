open Spotlib.Spot
open Utils
open Opamfind

val stdlib_dir : FilePath.t Lazy.t
(** The path of stdlib directory *)
  
val sw : Opamlib.Switch.t Lazy.t
(** The OPAM switch *)
  
val packages : Ocamlfind.Package.t list Lazy.t
(** OCamlFind packages found in the system *)

val opam_packages : Opamfind.Opamlib.Package.t list Lazy.t
(** OPAM packages found in the system *)
  
val ocamlfind_package_groups : Ocamlfind.Analyzed_group.t list Lazy.t
(** OCamlFind package groups found in the system *)
  
val ocamlfinds_of_opam : (Opamlib.Package.t * Ocamlfind.Analyzed_group.t list) list Lazy.t
(** OCamlFind packages provided by each OPAM package *)
  
val opams_of_ocamlfind : (Ocamlfind.Analyzed_group.t * Opamlib.Package.t list) list Lazy.t
(** The OPAM package which provides each OCamlFind package group *)
