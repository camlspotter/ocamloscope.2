(** Various tools *)

open Spotlib.Spot

open Typedtree
open Path
open Format

module Ident = struct
  include Ident

  let string_of ?(stamp=true) id =
    if stamp then Ident.unique_toplevel_name id
    else Ident.name id

  let ocaml_of_t id = Ocaml.String (string_of id)

  let is_predef id = id.Ident.stamp < 1000
end

module Path = struct
  include Path

  let rec string_of ?(stamp=true) = function
    | Pident id -> Ident.string_of ~stamp id
    | Pdot (t, n, i) when stamp = true -> string_of ~stamp t ^ "." ^ n ^ "/" ^ string_of_int i
    | Pdot (t, n, _i) -> string_of ~stamp t ^ "." ^ n
    | Papply (t1,t2) -> string_of ~stamp t1 ^ "(" ^ string_of ~stamp t2 ^ ")"

  let is_predef = function
    | Pident id -> Ident.is_predef id
    | _ -> false
        
  let rec is_global = function
    | Pident id -> if Ident.persistent id then Some id else None
    | Pdot (p, _, _) -> is_global p
    | Papply (p, _) -> is_global p

  let ocaml_of_t id = Ocaml.String (string_of id)
end

module Longident = struct
  include Longident

  let rec string_of = function
    | Lident s -> s
    | Ldot (l1,n) -> string_of l1 ^ "." ^ n
    | Lapply (l1,l2) -> string_of l1 ^ "(" ^ string_of l2 ^ ")"

  let rec of_path = let open Path in function
    | Pident id -> Lident id.Ident.name
    | Pdot (id, n, _) -> Ldot (of_path id, n)
    | Papply (p1, p2) -> Lapply (of_path p1, of_path p2)
      
  let rec of_rev_ids = function
    | [] -> assert false
    | [id] -> Lident (id.Ident.name)
    | id::rev_ids ->
        let lid = of_rev_ids rev_ids in
        Ldot (lid, id.Ident.name)

  let rec fake_path = function
    | Lident s -> Pident (Ident.create s)
    | Ldot (l1,s) -> Pdot (fake_path l1, s, 0)
    | Lapply (l1, l2) -> Papply (fake_path l1, fake_path l2)
        
end

let format_module_coercion ppf mc =
  let rec f ppf = function
    | Tcoerce_none -> string ppf "none"
    | Tcoerce_structure (ics, iics) ->
        fprintf ppf "@[<2>str@ [%a]@ [%a]@]"
          (list ";@ " (fun ppf (i,mc) ->
            fprintf ppf "(%d, %a)" i f mc)) ics
          (list ";@ " (fun ppf (id,i,mc) ->
            fprintf ppf "(%s, %d, %a)" (Ident.string_of id) i f mc)) iics
    | Tcoerce_functor (mc1, mc2) ->
        fprintf ppf "@[<2>functor@ (%a) ->@ (%a)@]"
          f mc1
          f mc2
    | Tcoerce_primitive _pc -> string ppf "primitive"
    | Tcoerce_alias (p, mc) ->
        fprintf ppf "@[<2>alias %s (%a)@]"
          (Path.string_of p)
          f mc
  in
  f ppf mc

module Location = struct
  include Location
  let ocaml_of_t l = Ocaml.String (Format.sprintf "%a" Location.print_compact l)
end
  
(*
module EnvSummary = struct
  open Env

  let format ppf env =
    let open Format in
    let rec f = function
      | Env_empty -> ()
      | Env_value (s, id, _) -> 
          fprintf ppf "val %s@," (Ident.name id);
          f s
      | Env_type (s, id, _) ->
          fprintf ppf "type %s@," (Ident.name id);
          f s
      | Env_extension (s, id, _) ->
          fprintf ppf "extension %s@," (Ident.name id);
          f s
      | Env_module (s, id, _) -> 
          fprintf ppf "module %s@," (Ident.name id);
          f s
      | Env_modtype (s, id, _) -> 
          fprintf ppf "module type %s@," (Ident.name id);
          f s
      | Env_class (s, id, _) ->
          fprintf ppf "class %s@," (Ident.name id);
          f s
      | Env_cltype (s, id, _) ->
          fprintf ppf "class type %s@," (Ident.name id);
          f s
      | Env_open (s, p) ->
          fprintf ppf "open %s@," (Path.name p);
          f s
      | Env_functor_arg (s, id) -> 
          fprintf ppf "functor arg %s@," (Ident.name id);
          f s
    in
    fprintf ppf "@[<v>";
    f & summary env;  
    fprintf ppf "@]"
end
*)

let (!!) = Lazy.(!!)

module Digest = struct
  open Typerep_lib.Std

  type t = string [@@deriving conv{ocaml}, typerep] (* = Digest.t in stdlib *)

  let cache = Hashtbl.create 101

  (** Cached [Digest.file]. XXX Probably we need auto flush. *)
  let file = Hashtbl.find_or_add Digest.file cache
  (* XXX do we need it? *)
end

exception Critical of string
let criticalf fmt = Exn.raisef (fun x -> Critical x) fmt
  
let command_name = "oco"

module FilePath = struct
  type t = string [@@deriving conv{ocaml}]
  (* This is a simple alias. Making it private requires lots of code change... *)
end

(** Scan the directory [d] and returns the first file which stafisfies [p] *)
let find_file_in_dir d p =
  Unix.Find.fold [d] None & fun st path ->
    match st with
    | Some _ -> `Prune, st
    | None -> if p path then `Prune, Some path else `Continue, None

(** xxx.cmo => /yyy/zzz/xxx.cmo *)

(*
val find_in_path : FilePath.t list -> FilePath.t -> FilePath.t
(** [find_in_path dirs m] finds the file [m] in the load path [dirs].
    If not found it raises [Critical] error.
*)
*)
let find_in_path dirs m =
  try
    Misc.find_in_path dirs m
  with
  | Not_found ->
      !!% "@[<2>Error: %s is not found in@ @[%a@]@]@." m Format.(list "@ " string) dirs;
      criticalf "Error: %s is not found in %s"
        m
        (String.concat " " dirs)

(** [Config.load_path] must be set properly. 
    No check of persistency of Ident.t
*)
let find_global_module id =
  (* Note that Misc.find_in_path_uncap finds "x.cmi" in Cygwin for "X.cmi",
     even if the real file name is "X.cmi".
  *)
  try
    Some (Misc.find_in_path_uncap !Config.load_path (id.Ident.name ^ ".cmi"))
  with
  | Not_found -> None

let module_name p = 
  String.capitalize_ascii & fst & Filename.(split_extension & basename p)

let around_if b start end_ f = 
  if b then start ();
  let res = f () in
  if b then end_ ();
  res

(** "some.package" => "some"
    "other" => "other"
*)
let top_package_name x = 
  match String.index x '.' with
  | exception _ -> x
  | n -> String.sub x 0 n
