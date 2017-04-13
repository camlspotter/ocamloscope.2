(** Flattened version of Hump *)

open Spotlib.Spot
open Utils
open List
open Sig
open Hump

type ent = (k * path) * v
  [@@deriving conv{ocaml_of}]

let flatten p me = 
  let rec mexp p me = match me with
    | ESignature sg -> sig_ p sg 
    | EFunctor ((_k,n), _, x) ->
        let p = Oide_apply (p, Oide_ident n) in
        mexp p x
    | ECoerce (_,x) -> mexp p x
    | ELet (_, _, x) -> mexp p x
    | EAddAlias (v, x) -> map (fun (kp,v') -> kp, Aliased (v, v')) & mexp p x

    (* Strange, but 
         EDot(ESignature([id,EModule(..)]),KModule,id) => EModule(..) 
                                                       =/=> ESignature ..
    *)
    | EModule (_,x) -> mexp p x
    | EModtype (_, Some x) -> mexp p x

    | EUnknownPath _ -> []
        
    | _ ->
(*
        !!% "WARNING: @[<2>Impos mexp:@ %a@ @[%a@]@]@." Hump.format_path p Hump.format me;
        assert false 
*)
        !!% "WARNING: @[<2>Impos mexp:@ %a ...@." Hump.format_path p;
        [] (* XXX workaround *)
          
(*
      if some special condition then !!% "WARNING: @[<2>Impos:@ %a @[%a@]@]@." Hump.format_path p Hump.format x;
*)

  and sig_ p sg = flip concat_map sg & fun ((k,n),x) ->
    let p0 = p in
    let p = Oide_dot (p, n) in
    let rec sigitem si = match si with
      | EModule (v, x) -> ((k,p),v) :: mexp p x
      | EModtype (v, Some x) -> ((k,p),v) :: mexp p x
      | ESignature _ ->
          (* ESignature [ (KModule, n), ESignature ... ]
             is possible, when n is packed
          *)
          ((k,p),LocNone) :: mexp p si
      | EType (v, xs) -> ((k,p),v) :: sig_ p0 xs
      | EClass (v, xs) -> ((k,p),v) :: sig_ p xs
      | EModtype (v, None)
      | ETypext v
      | EValue v
      | EClasstype (v, _) 
      | EConstructor v 
      | EField v
      | EMethod v -> [((k,p),v)]
      (* F(N) = struct module M = N end 
         has
         (KModule, "M"),  ECoerce (EVar ..., ESignature ...) *)
      | ECoerce (_,x) -> ((k,p),LocNone) :: sigitem x
      | EWith (_,x) -> ((k,p),LocNone) :: sigitem x (* not sure... *)
      | EAddAlias (v, x) ->
          map (fun (kp,v') -> kp, Aliased (v, v')) & sigitem x
      | ELet (_,_,x) -> sigitem x

      | EUnknownPath _ -> []
            
(*
      | EDot _ -> [] (* hmmm. EDot (ELet(true,...)): this can be fixed *)
*)

      | _ ->
(*
          !!% "WARNING: @[<2>Impos sitem:@ %a@ @[%a@]@]@." Hump.format_path p Hump.format si;
           assert false 
*)
         !!% "WARNING: @[<2>Impos sitem:@ %a@ ...@]@." Hump.format_path p;
         [((k,p),LocNone)] (* XXX workaround *)
    in
    sigitem x
  in
  ((KModule, p), Def { kind= KModule
                     ; path= p
                     ; loc= Location.none
                     ; digest= None
                     ; doc= None (*XXX*)
                     }) (* XXX good Hump.v for the entire file *)
  :: mexp p me

let flatten p me =
  try flatten p me with e ->
    !!% "@[<2>Flatten failed:@ @[%a@]@]@."
      Hump.format me;
    raise e

let load_hump datadir n =
  match find_opt (fun p -> p.Opamfind.Ocamlfind.Analyzed_group.name = n) !!Package.ocamlfind_package_groups with
  | None -> assert false
  | Some apg -> (Humpscrape.scrape_ocamlfind_package datadir false apg).Data.HumpFile.humps

let cached_hump_loader datadir =
  let cache = Hashtbl.create 17 in
  Hashtbl.find_or_add (load_hump datadir) cache

let global_source_of_cached_hump_loader ~loader p =
  let rec get_name = function
    | Oide_ident "UNRESOLVED" ->
        (* XXX warn? *)
        None
    | Oide_ident "predef" -> Some "predef" (* XXX correct? *)
    | Oide_ident s when is_package_path_name s ->
        begin match Packpath.parse s with
        | None -> assert false
        | Some [] -> assert false
        | Some (x::_) ->
            match String.split (function '.' -> true | _ -> false) x with
            | [] -> assert false
            | x::_ -> Some x
        end
    | Oide_dot (p, _) -> get_name p
    | x -> !!% "global_source_of_cached_hump_loader: %a@." Hump.format_path x; assert false
  in
  let open Option in
  get_name p >>= fun name -> loader name |> assoc_opt p

let load_package ~datadir =
  let loader = cached_hump_loader datadir in
  fun p -> loader p
      
(** Evaluate the hump of package [p] loading necessary cmtz files from [datadir] *)
let eval_package ~datadir p =
  let loader = cached_hump_loader datadir in
  let global_source = global_source_of_cached_hump_loader ~loader in
  let module E = Humpeval.Make(struct 
    let global_source = global_source 
    let go_on_even_at_coercion_errors = false
  end) in
  let xs = loader p in
  (* XXX we can evaluate from (path,e) directly.. *)
  map (fun (path,_) -> path, from_Some & E.eval_global path) xs

let verbose = ref false

let flatten_package ~datadir p =
  let xs = eval_package ~datadir p in
  (* if !verbose then !!% "%a@." (Ocaml.format_with [%derive.ocaml_of: (Hump.path * Hump.expr) list]) xs; *)
  let ys = map (fun (path, e) -> path, flatten path e) xs in
  !!% "@.FLAT!!!@.";
  if !verbose then
    iter (!!% "%a@." (Ocaml.format_with [%derive.ocaml_of: path * ent list])) ys;
  ys
  
let test_stdlib datadir =
  let xs = load_hump datadir "stdlib" in
  flip iter xs & fun (path, e) ->
    !!% "%a@."
      (Ocaml.format_with [%derive.ocaml_of: ent list])
      (flatten path e)
      
