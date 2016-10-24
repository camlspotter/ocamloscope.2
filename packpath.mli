(** Module to handle package names as paths, ex. {stdlib}. *)
  
open Opamfind

val make_from_names : string list -> string
(** Make a unique path name for given OCamlFind package names,
    like ["{compiler-libs.commn,compiler-libs.bytecomp}"].
*)
  
val make : Ocamlfind.Analyzed.t list -> string
(** Make a unique path name for given OCamlFind package list,
    like ["{compiler-libs.commn,compiler-libs.bytecomp}"].
*)
  
val parse : string -> string list option
(** Parse the result of [make] and returns package name components. 

  [parse "{compiler-libs.common,compiler-libs.bytecomp}" = Some ["compiler-libs.common"; "compiler-libs.bytecomp"]]
*)
