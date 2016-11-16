open Spotlib.Spot
open List
open Sig
open Data
open Outcometree

let _debug = ref false

let () = Lexer.init () (* We need this ... *)

type q = 
  | Path of out_ident
  | Path_type of out_ident * out_type
  | Type of out_type
[@@deriving conv{ocaml_of}]

type t = k option * q
[@@deriving conv{ocaml_of}]

let parse_as_type s =
  try
    let lexbuf = Lexing.from_string s in
    let core_type = Parse.core_type lexbuf in
    Some (Type (Coretype.out_of_core_type core_type))
  with
  | _ -> None

(* id : type *)
let parse_as_path_type s =
  try
    let s = "(" ^ s ^ ")" in
    let lexbuf = Lexing.from_string s in
    let e = Parse.expression lexbuf in
    match e.pexp_desc with
    | Pexp_constraint ({pexp_desc= Pexp_ident {txt=lid}}, ty) ->
        Some (Path_type(Coretype.out_of_longident lid, Coretype.out_of_core_type ty))
    | _ -> None
  with
  | _ -> None

(* _ : type *)
let parse_as_wildcard_colon_type s =
  try
    let s = "( x" ^ s ^ ")" in
    let lexbuf = Lexing.from_string s in
    let e = Parse.expression lexbuf in
    match e.pexp_desc with
    | Pexp_constraint ({pexp_desc= Pexp_ident {txt=Lident "x_"}}, ty) ->
        Some (Type(Coretype.out_of_core_type ty))
    | _ -> None
  with
  | _ -> None

let parse_path s =
  let f s = 
    try
      let lexbuf = Lexing.from_string s in
      let e = Parse.expression lexbuf in
      match e.pexp_desc with
      | Pexp_constraint ({pexp_desc= Pexp_ident {txt=lid}}, _ty) -> Some (Path (Coretype.out_of_longident lid))
      | Pexp_constraint ({pexp_desc= Pexp_construct ({txt=lid}, _)}, _ty) -> Some (Path (Coretype.out_of_longident lid))
      | _ -> None
    with
    | _ -> None
  in
  match f ("(" ^ s ^ ": int)") with
  | Some x -> Some x
  | None -> f ("(( " ^ s ^ " ) : int)")

module Re = Ppx_orakuda.Regexp.Re_pcre
open Re.Infix
open Re.Literal
    
(* trouble is that "module type" and "class type" have space inside. *)
let parse_kind_prefix s = case s
  |> ( {m|^\s*(val|type|typext|exception|module|module\s+type|class|class\s+type|field|constructor|method)\s+|m} ==> fun r ->
    let k = from_Some & Sig.parse_k r#_1 in
    Some (k, r#_right)
     )
  |> default (fun () -> None)

let parse s =
  let prefixed = match parse_kind_prefix s with
    | None -> []
    | Some (k, s) ->
        let parse_with_prefix s =
          filter_map id [ parse_as_path_type s; parse_path s; parse_as_wildcard_colon_type s ]
        in
        map (fun x -> Some k, x) & parse_with_prefix s
  in
  let non_prefixed =
    let parse_without_prefix s =
      filter_map id [ parse_as_path_type s; parse_as_type s; parse_path s; parse_as_wildcard_colon_type s ]
    in
    map (fun x -> None, x) & parse_without_prefix s
  in
  let qs = prefixed @ non_prefixed in
  let qs, warning =
    (* There are two ways to parse  map : ('a -> 'b) -> 'a list -> 'b list :

       * something named map of type ('a -> 'b) -> 'a list -> 'b list
       * something of type map:('a -> 'b) -> 'a list -> 'b list

       We discard the latter possibility and propose the user to write
       "_ : map:('a -> 'b) -> 'a list -> 'b list" if he/she really wants it.
    *)
    let ty = filter (function (None, Type _) -> true | _ -> false) qs in
    let path_ty = filter (function (None, Path_type _) -> true | _ -> false) qs in
    let path = filter (function (None, Path _) -> true | _ -> false) qs in
    let with_kind = filter (function (Some _, _) -> true | _ -> false) qs in
    assert (length qs = length (ty @ path_ty @ path @ with_kind));
    match ty, path_ty with
    | _::_, _::_ ->
        path_ty @ path @ with_kind,
        ["Use \"_ : " ^ s ^ "\", if you want to search a function with a labeled argument"]
    | _ -> ty @ path_ty @ path @ with_kind, []
        
  in
  qs, warning

module PackageSpec = struct    
  type t =
    | Just of string list
    | All_but of string list
    | Vanilla of string list
  [@@deriving conv{ocaml_of}]
  
  (** packages of "[distributed with Ocaml..." - compiler-libs and ocamldoc *)
  let vanilla =
    [ "predef"
    ; "bigarray"
    ; "bytes"
    ; "dynlink"
    ; "graphics"
    ; "num"
    ; "stdlib"
    ; "str"
    ; "threads"
    ; "unix"
    ]
  
  let check pspec toppack =
    match pspec with
    | Just ps -> mem toppack ps
    | All_but ps -> not & mem toppack ps
    | Vanilla ps -> mem toppack (ps @ vanilla)
end
    
let query {DB.items= fs_t_list; top_types= ts} pspec qs0 = 
  let module Match = Sigmatch.Make(struct
    let cache = Levenshtein.StringWithHashtbl.create_cache 1023
  end) in

  (* prepare type pools for qs *)

  let qs = flip map qs0 & fun (ko,s) ->
    (ko,s, match s with Path _ -> Array.create 0 `None | _ -> Array.(create (length ts) `None))
  in

  let query_one max_dist (ko,s,cache) {DB.kind=k;top_pack;path=p;tyid=tio} =
    if not & PackageSpec.check pspec top_pack then None
    else
    match ko, k with
    | Some k', k when k <> k' -> None
    | _ ->
    match s, tio with
    | (Type _ | Path_type _), None -> None
    | _ ->
        let open Option in
        let%m (dist, path_trace) = match s with
          | Path pat_p | Path_type (pat_p, _) ->
              begin match Match.match_path pat_p p max_dist with
              | None -> None
              | Some (dist, trace) -> Some (dist, Some trace)
              end
          | _ -> Some (max_dist, None)
        in
        let%m dist, type_trace = match s, tio with
          | Path _, _ -> Some (dist, None)
          | _, None -> None
          | (Type pat_t | Path_type (_, pat_t)), Some ti ->
              match Array.unsafe_get cache ti with
              | `Exact (dist', _) when dist < dist' -> 
                  None
              | `Exact (dist', trace) -> 
                  Some (dist', Some trace)
              | `Morethan dist' when dist <= dist' -> 
                  None
              | `None | `Morethan _ ->
                  begin match Match.match_type pat_t (Array.unsafe_get ts ti) dist with
                  | None ->
                      Array.unsafe_set cache ti (`Morethan dist);
                      None
                  | Some (dist, trace) ->
                      Array.unsafe_set cache ti (`Exact (dist, trace));
                      Some (dist, Some trace)
                  end
        in
        Some (dist, path_trace, type_trace)
  in

  let query_entry max_dist qs dbitem =
    let res = fold_left (fun res q ->
      let max_dist = match res with
        | Some (d,_,_) -> max 0 (d-1)
        | None -> max_dist
      in
      query_one max_dist q dbitem) None qs
    in
    match res with
    | None -> None
    | Some x -> Some (dbitem, x)
  in

  let module P = Xprinttyp.Make(struct let rewrite = Utils.out_ident_of_path end) in
  
  let dthresh = Distthresh.create ~thresh:5 ~limit:100 in
  let dthresh = flip2 fold_left dthresh fs_t_list & fun dthresh dbitem ->
    match query_entry (Distthresh.thresh dthresh) qs dbitem with
    | None -> dthresh
    | Some (ent,(d,pt,tt)) -> Distthresh.add dthresh d (ent,pt,tt)
  in
  Distthresh.to_list dthresh
