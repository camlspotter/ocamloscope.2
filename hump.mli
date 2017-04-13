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
  | Def     of def
  | Aliased of v * v (** [Aliased (v1, v2)]: thing defined originally at v2 is aliased at v1 *)
  | Coerced of v * v (** [Coerced (v1, v2)]: [v1] is coerced by [v2] *)
  | LocNone
  | Prim of string (** ex. "%addint" *)

and def = { kind   : k
          ; path   : path
          ; loc    : location
          ; digest : Digest.t option (** source file digest *)
          ; doc    : string option
          }

and id = k * string

and expr =
  | EGVar        of path
    (** Persistent global module *)
  | EVar         of k * string
    (** Variable *)
  | EDot         of expr * k * string
    (** Field access *)
  | ELet         of bool * (id * expr) list * expr
    (** Let binding. The bool indicates recursion. *)
  | ESignature   of (id * expr) list
    (** Signature *)
  | EApply       of expr * expr
    (** Functor application *)
  | EFunctor     of id * expr option * expr
    (** Functor *)
  | ECoerce      of expr * expr
    (** Signature coercion. [ECoerce (m, n)]: [m] is coerced by [n] *)
  | EWith        of expr * expr
    (** Signature constraints. [EWith (m, n)]: module type m = n with ...  m must be a subtype of n *)
  | EModule      of v * expr
    (** [module M = ..] *) 
  | EModtype     of v * expr option
    (** [module type S = ..] *)
  | EType        of v * (id * expr) list
    (** [type t = ..] *) 
  | ETypext      of v
    (** [type t += ..] *)
  | EValue       of v
    (** [val x : ...] *)
  | EClass       of v * (id * expr) list
    (** [class c = object .. end] *)
  | EClasstype   of v * (id * expr) list
    (** [class type ct = object .. end] *)
  | EConstructor of v
    (** Variant constructor *)
  | EField       of v
    (** Record field *)
  | EMethod      of v
    (** Class method *)
  | EUnknownPath of path
    (** Unresolved path *)
  | EAnnotate    of string * expr
    (** Annotate a string to an expression, mainly for debugging. *)
  | ERecM        of id * (id * expr option) list * (id * expr) list
    (** Recursive module: [module rec M : sig .. end = struct .. end] *)  
  | EAddAlias    of v * expr
    (** [EddAlias (v,e)] explicitly adds [(Aliased (v,_)] *)
  | EError       of string
[@@deriving conv{ocaml_of}, typerep]

val format : Format.t -> expr -> unit

val get_doc : v -> string option
(** Get the best docstring. *) (* XXX not well tested *)
  
val print_v : Format.t -> v -> unit
(** Human friendly printer of [v] *)
  
