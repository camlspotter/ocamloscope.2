open Spotlib.Spot
open Utils
open Sig
open Typerep_lib.Std

type path = [%import: Outcometree.out_ident]
and position = [%import: Lexing.position]
and location = Location.t = { loc_start: position; loc_end: position; loc_ghost: bool }
[@@deriving conv{ocaml_of}, typerep]

(* human friendly printing *)
let ocaml_of_location = Location.ocaml_of_t

(* human friendly printing *)
let ocaml_of_path =
  let module M = Xprinttyp.Make(struct let rewrite = id end) in
  fun oi -> Ocaml.String (M.string_of_out_ident oi)

let string_of_path x =
  (* Eta is a must! Otherwise the output becomes crazy! *)
  Format.sprintf "%a" (Ocaml.format_with ocaml_of_path) x
  
let format_path ppf x = Ocaml.format_with ocaml_of_path ppf x
  
(* The language is too general, but it is easier to consider for me... *)

type v =
  | Def of path * location * Digest.t option (* source digest *)
  | Aliased of v * v (*+ Aliased (v1, v2): thing defined originally at v2 is aliased at v1 *)
  | Coerced of v * v
  | LocNone
  | Prim of string (*+ ex. "%addint" *)
and id = k * string
and expr =
  | EGVar of path (* persistent module *)
  | EVar of k * string
  | EDot of expr * k * string
  | ELet of bool * (id * expr) list * expr
  | ESignature of (id * expr) list
  | EApply of expr * expr
  | EFunctor of id * expr option * expr
  | ECoerce of expr * expr
  | EWith of expr * expr (* EWith (m, n): module type m = n with ...  m must be a subtype of n *)
  | EModule of v * expr
  | EModtype of v * expr option
  | EType of v * (id * expr) list
  | ETypext of v
  | EValue of v (* We see lots of EValue LocNone therefore we should hashcons *)
  | EClass of v * (id * expr) list
  | EClasstype of v * (id * expr) list
  | EConstructor of v
  | EField of v
  | EMethod of v
  | EUnknownPath of path
  | EAnnotate of string * expr
  | ERecM of id * (id * expr option) list * (id * expr) list
  | EAddAlias of v * expr
[@@deriving conv{ocaml_of}, typerep]

let format ppf = Ocaml.format_with ocaml_of_expr ppf
