open Spotlib.Spot

module Ident : sig
  include module type of struct include Ident end

  val string_of : ?stamp: bool -> t -> string
  (** If [stamp = true], the integer stamp is printed *)

  val is_predef : t -> bool
  (** Returns [true] when the identifier is predefined one *)

  val ocaml_of_t : t -> Ocaml.t
end

module Path : sig
  include module type of struct include Path end

  val string_of : ?stamp: bool -> t -> string
  (** If [stamp = true], the integer stamp is printed *)

  val is_predef : t -> bool
  (** Returns [true] when the path is a predefined one *)

  val is_global : t -> Ident.t option
  (** Returns [Some g] when the path starts from a global identifier [g] *)

  val ocaml_of_t : t -> Ocaml.t
end

module Longident : sig
  include module type of struct include Longident end

  val string_of : t -> string

  val of_path : Path.t -> t

  val fake_path : t -> Path.t
end

val format_module_coercion
  : Format.formatter -> Typedtree.module_coercion -> unit

module Location : sig
  include module type of struct include Location end

  val ocaml_of_t : t -> Ocaml.t
end

val (!!) : 'a Lazy.t -> 'a
(** Alias of [Lazy.force] *)

module FilePath : sig
  type t = string [@@deriving conv{ocaml}]
end

module Digest : sig
  type t = Digest.t [@@deriving conv{ocaml}(* , typerep *)]

  val file : FilePath.t -> Digest.t
  (** The original [Digest.file] with cache. *)

  val reset_cache : unit -> unit
  (** Reset the cache for [file] *)
end

exception Critical of string

val criticalf : ('a, unit, string, 'b) format4 -> 'a

val command_name : string
(** "oco" *)

val find_file_in_dir
  : FilePath.t
  -> (Unix.Find.path -> bool)
  -> Unix.Find.path option

val find_in_path : FilePath.t list -> FilePath.t -> FilePath.t
(** Same as [Misc.find_in_path] of compiler-libs, but with some error
    printing if failed.
*)

val find_global_module : Ident.t -> FilePath.t option

val module_name : string -> string
(** "xxx/yyy/zzz.cmi" => "Zzz" *)

val around_if
  : bool
  -> (unit -> unit)
  -> (unit -> unit)
  -> (unit -> 'a)
  -> 'a
(** [around_if b pre post f], if [b = false], just executes [f ()].
    If [b = true], it executes [pre (); f (); post ()] and returns
    the result of [f ()].
*)

val top_package_name : string -> string
(** "some.package" => "some"
    "other" => "other"
*)

val is_package_path_name : string -> bool
(** Return [true] if the string starts with ['{'] *)

val out_ident_of_path : Path.t -> Outcometree.out_ident
