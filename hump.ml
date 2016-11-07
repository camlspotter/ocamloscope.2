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
  | Def of def
  | Aliased of v * v (* Aliased (v1, v2): thing defined originally at v2 is aliased at v1 *)
  | Coerced of v * v
  | LocNone
  | Prim of string (*+ ex. "%addint" *)
and def = { path   : path
          ; loc    : location
          ; digest : Digest.t option (** source file digest *)
          ; doc    : string option
          }
and id = k * string
and expr =
  | EGVar of path
  | EVar of k * string
  | EDot of expr * k * string
  | ELet of bool * (id * expr) list * expr
  | ESignature of (id * expr) list
  | EApply of expr * expr
  | EFunctor of id * expr option * expr
  | ECoerce of expr * expr
  | EWith of expr * expr
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

let rec get_doc = function
  | Prim _ | LocNone -> None
  | Def d -> d.doc
  | Aliased (v1,v2) ->
      begin match get_doc v1 with
      | None -> get_doc v2
      | Some d -> Some d
      end
  | Coerced (v1,v2) ->
      match get_doc v2 with
      | None -> get_doc v1
      | Some d -> Some d

let rec print_v ppf =
  let open Format in
  function
    | Def d -> fprintf ppf "%a at %a"
        (Ocaml.format_with ocaml_of_path) d.path (* XXX make the printer as a library function *)
        Location.print_compact d.loc
    | Aliased (v1, v2) ->
        fprintf ppf "@[<v2>Aliased at@ %a@ org-defined at %a@]" print_v v1 print_v v2
    | Coerced (v1, v2) ->
        fprintf ppf "@[<v2>Coerced @ %a @ by %a@]" print_v v1 print_v v2
    | LocNone -> string ppf "LocNone"
    | Prim s -> fprintf ppf "Primitive %s" s
