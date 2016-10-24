(** Abstraction of *.ml/mli, saved to *.hump files. *)

open Spotlib.Spot
open Utils
open Sig

type path = Sig.out_ident [@@deriving conv{ocaml_of}, typerep]

val string_of_path : path -> string
val format_path : Format.t -> path -> unit

type position = [%import: Lexing.position]
and location = Location.t = { loc_start: position; loc_end: position; loc_ghost: bool }
  [@@deriving conv{ocaml_of}, typerep]

type v =
  | Def     of path * location * Digest.t option
  | Aliased of v * v (*+ Aliased (v1, v2): thing defined originally at v2 is aliased at v1 *)
  | Coerced of v * v
  | LocNone
  | Prim of string (*+ ex. "%addint" *)

and id = k * string

and expr =
  | EGVar        of path (** persistent module *)
  | EVar         of k * string
  | EDot         of expr * k * string
  | ELet         of bool * (id * expr) list * expr
  | ESignature   of (id * expr) list
  | EApply       of expr * expr
  | EFunctor     of id * expr option * expr
  | ECoerce      of expr * expr
  | EWith        of expr * expr (* EWith (m, n): module type m = n with ...  m must be a subtype of n *)
  | EModule      of v * expr
  | EModtype     of v * expr option
  | EType        of v * (id * expr) list
  | ETypext      of v
  | EValue       of v (* We see lots of EValue LocNone therefore we should hashcons *)
  | EClass       of v * (id * expr) list
  | EClasstype   of v * (id * expr) list
  | EConstructor of v
  | EField       of v
  | EMethod      of v
  | EUnknownPath of path
  | EAnnotate    of string * expr
  | ERecM        of id * (id * expr option) list * (id * expr) list
  | EAddAlias    of v * expr
[@@deriving conv{ocaml_of}, typerep]

val format : Format.t -> expr -> unit
