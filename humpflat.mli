(** Flatten Hump data *)

open Utils
open Sig
open Hump

val verbose : bool ref
(** Make things in this module more verbose *)
  
type ent = (k * path) * v
  [@@deriving conv{ocaml_of}]

val flatten
  : Hump.path
  -> Hump.expr
  -> ent list

(** [flatten_package ~datadir package].
    [datadir]/[package].cmtz must exist.
*)
val flatten_package
  : datadir: FilePath.t
  -> string (*+ package name such as "stdlib" *)
  -> (Hump.path * ent list) list

val load_package
  : datadir: FilePath.t
  -> string (*+ package name *)
  -> (Hump.path * Hump.expr) list

val eval_package
  : datadir:FilePath.t
  -> string (*+ package name *)
  -> (Hump.path * Hump.expr) list

val test_stdlib : FilePath.t (*+ data dir *) -> unit
