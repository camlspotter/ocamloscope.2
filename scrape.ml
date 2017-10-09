(** Sig meets Hump: combine type informations from cmi files and
    module alias analysis
*)

open Spotlib.Spot
open Utils
open List
open Sig
open Data

(* Remove the ident postfix introduced for alpha-conversion of
   functor argument names *)
let remove_alpha_postfix ((k,path),v) =
  let rec pconvert = function
    | Oide_apply(p1, p2) -> Oide_apply (pconvert p1, pconvert p2)
    | Oide_dot(p, n) -> Oide_dot(pconvert p, n)
    | Oide_ident s as i ->
        match String.index s ' ' with
        | exception _ -> i
        | p -> Oide_ident (String.sub s 0 p)
  in
  ((k, pconvert path), v)

(* Forget the name of arguments in functor applications: F(SomeModule) => F(x)

   This normalization is required since functor argument names can vary 
   between Sigs and Humps, like F(X) and F(Y) 
*)
let rec normalize_path p = match p with
  | Oide_ident _ -> p
  | Oide_apply (p1, Oide_ident _) -> Oide_apply (normalize_path p1, Oide_ident "x")
  | Oide_apply _ -> assert false
  | Oide_dot (p,n) -> Oide_dot (normalize_path p, n)

let rec get_alias = function
  | Hump.Def { kind; path=p } -> Some (Data.Path (kind, p))
  | Prim n           -> Some (Data.Primitive n)
  | Aliased (_, v)   -> get_alias v
  | Coerced (v, _)   -> get_alias v
  | LocNone          -> None

let combine_sig_hump sigs (humps : Humpflat.ent list) =
  let humps = map remove_alpha_postfix humps in
  let humps = Hashtbl.of_list 101 & map (fun ((k,p),v) -> (k,normalize_path p), v) humps in

  flip map sigs & fun ((k,p),v) ->
    let p' = normalize_path p in
    let vo = Hashtbl.find_opt humps (k,p') in
    let alias =
      Option.bind vo & fun v -> 
        let a = get_alias v in
        (* remove self alias *)
        if a <> Some (Data.Path (k,p)) then a else None
    in
    (k,p),(v,alias,vo)

let is_cached datfile stamp =
  if File.Test._f datfile then
    let dat = Dat.load datfile in
    if dat.Dat.stamp = stamp then Some dat else None
  else None
    
(* sig + hump = dat *)
let package destdir p =
  !!% "Scrape.package %s...@." p.Opamfind.Ocamlfind.Analyzed_group.name;
  let open Opamfind.Ocamlfind in
  let version = match p.Analyzed_group.packages with
    | [] -> assert false
    | p::_ -> Package.version p.Analyzed.package
  in
  let n = p.Opamfind.Ocamlfind.Analyzed_group.name in
  let ms = Result.from_Ok & Cm.traverse_packages p in
  let stamp = Cm.package_stamp ms in
  match is_cached (destdir ^/ n ^ ".dat") stamp with
  | Some x -> x
  | None ->
      let sigfile = Sigscrape.scrape_ocamlfind_package destdir p in
      let sig_ = concat_map snd sigfile.SigFile.packs in
      let humpents = concat_map snd & Humpflat.flatten_package ~datadir:destdir n in
      let items = combine_sig_hump sig_ humpents in
      let dat = { Dat.name = n; items; stamp; version } in
      Dat.save (destdir ^/ n ^ ".dat") dat;
      dat

let packages datadir packs =
  let packs = match packs with
    | [] -> !!Package.ocamlfind_package_groups
    | _ ->
        filter (fun apg -> mem apg.Opamfind.Ocamlfind.Analyzed_group.name packs) !!Package.ocamlfind_package_groups
  in
  iter (ignore *< package datadir) packs
