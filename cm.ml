open Spotlib.Spot
open Utils
open List
open Opamfind
open Outcometree

(* cm files *)

type t = {
  paths      : Hump.path list; (** one cm file may be accessible in more than one way. The best path comes at the head *)
  digest     : Digest.t; (** digest of cmi *)
  cmi        : FilePath.t; (** cmi file location. This must always exist. *)
  cmt        : FilePath.t option; (** cmt file location *)
  cmti       : FilePath.t option; (** cmti file location *)
  ocamlfinds : (Ocamlfind.Analyzed_group.t * Ocamlfind.Analyzed.t) list;
  opam       : Opam.Package.t option
}
[@@deriving conv{ocaml_of}]

module Summary = struct
  type t = {
    paths      : Hump.path list; (** one cm file may be accessible in more than one way *)
    cmi        : FilePath.t; (** cmi file location. This must always exist. *)
    cmt        : FilePath.t option; (** cmt file location *)
    cmti       : FilePath.t option; (** cmti file location *)
    ocamlfinds : string list;
    opam       : string option;
  }
  [@@deriving conv{ocaml_of}]

  let format ppf = Ocaml.format_with ocaml_of_t ppf

end

let summary t =
  { Summary.paths = t.paths;
    cmi = t.cmi;
    cmt = t.cmt;
    cmti = t.cmti;
    ocamlfinds = map (fun (_,ap) -> Ocamlfind.Analyzed.name ap) t.ocamlfinds;
    opam = Option.fmap (fun x -> x.Opam.Package.name) t.opam
  }

let format ppf x = Summary.format ppf & summary x

module Cache = struct
  open Opamfind.Utils

  let verbose = ref false
    
  let cmfile_cache = Hashtbl.create 17
  
  let cmfiles_in_dir dir =
    Hashtbl.find_or_add (fun dir ->
      around_if !verbose 
        (fun () -> !!% "Scanning cmfiles in %s ...@." dir)
        (fun () -> !!% "Scanning done@.")
        & fun () ->
          Unix.Find.fold [dir] [] & fun st path ->
            match snd (Filename.split_extension path#base) with
            | ".cmi" | ".cmo" | ".cmx" | ".cma" | ".cmxa" | ".cmxs" | ".cmt" | ".cmti" ->
                `Continue, ((path, lazy (Digest.file path#path)) :: st)
            | ".hg" | ".git" -> `Prune, st
            | _ -> `Continue, st
    ) cmfile_cache dir
  
  let find_the_same_files p cmfiles =
    let base = Filename.basename p in
    let d = Digest.file p in
    flip filter_map cmfiles & fun (p,dz) ->
      if p#base = base && d = !!dz then Some p#path else None
  
  let reset_cache () =
    Hashtbl.clear cmfile_cache
end

let non_opam_dirs = ref ([] : FilePath.t list)

let non_opam_cmis = lazy begin
  !!% "Scanning out of OPAM source directories specified by --src-dir.  It may take really long time...@.";
  let res = 
    flip concat_map !non_opam_dirs & fun d ->
      flip filter (Cache.cmfiles_in_dir d) & fun (p, _) ->
        snd (Filename.split_extension p#base) = ".cmi"
  in
  !!% "Scanned out of OPAM source directories@.";
  res
end

let find_cms srcdir p =
  let cmi = Filename.change_extension ~ext:".cmi" p in
  if not & File.Test._f cmi then `Error (`Cmi_file_does_not_exist cmi)
  else
    (* XXX We do not check mli only module. We are pretty optimistic here:
       if cmti exists and no cmt, then it is mli only. 
    *)
    let cmt, cmti = 
      let compatible_cmis = lazy begin 
        let cmfiles = match srcdir with
          | None ->
              !!% "Finding %s in non opam cmis@." p;
              !!non_opam_cmis
          | Some dir -> Cache.cmfiles_in_dir dir
        in
        Cache.find_the_same_files cmi cmfiles
      end in
      let find ~ext p =
        (* We prefer the cmt/cmti files in the build directory *)
        let find_original cmt0 = 
          let open Cmt_format in
          let _, ext = Filename.split_extension cmt0 in
          match Cmt_format.read cmt0 with
          | _, Some cmt_infos ->
              begin match cmt_infos.cmt_sourcefile with
              | Some f ->
                  let src = cmt_infos.cmt_builddir ^/ f in
                  let cmt = Filename.change_extension ~ext src in
                  if File.Test._f cmt then cmt else cmt0
              | None -> cmt0
              end
          | _ -> assert false
        in
        let cmt = Filename.change_extension ~ext p in
        if File.Test._f cmt then Some (find_original cmt)
        else
          flip find_map_opt !!compatible_cmis & fun cmi ->
            let cmt  = Filename.change_extension ~ext cmi in
            if File.Test._f cmt then Some cmt else None
      in
      let cmt  = find ~ext:".cmt"  p in
      let cmti = find ~ext:".cmti" p in
      cmt, cmti
    in
    `Ok (cmi, cmt, cmti)

let get_packed cmt_infos =
  match cmt_infos.Cmt_format.cmt_annots with
  | Packed (_, ss) ->
      (* ss maybe xxx.cmo or xxx.cmx *)
      let open Cmt_format in
      let dirs = map (fun s -> cmt_infos.cmt_builddir ^/ s) cmt_infos.cmt_loadpath in
(*
      !!% "@[<2>Found a packed module %s. Searching sub-modules@ @[%a@] in@ @[%a@]@]@."
        cmt
        Format.(list "@ " string) ss
        Format.(list "@ " string) dirs;
*)
      map Filename.(change_extension ~ext:".cmi" *< find_in_path (cmt_infos.cmt_builddir :: dirs)) ss
  | Partial_implementation _ ->
      criticalf "Error: %s is not compiled successfully" cmt_infos.cmt_modname
  | Implementation _ -> []
  | Interface _ -> assert false
  | Partial_interface _ -> assert false

(* XXX it will scan things many times if there are more than one ocamlfind
   sub-packages, but who cares? (for now) *)
let traverse_package srcdir apg ap =
  let cmis = Ocamlfind.accessible_cmis apg ap in (* starts from here *)
  let rec f st = function
    | [] -> st
    | (parent_path, cmi)::pcmis ->
        let path = Oide_dot (parent_path, module_name cmi) in
        match find_cms srcdir cmi with
        | `Error _ -> assert false
        | `Ok (_, cmt, cmti) ->
            let new_pcmis = match cmt with
              | Some cmt -> map (fun cmi -> path, cmi) & get_packed & Cmt_format.read_cmt cmt
              | None -> []
            in
            f ((path, Digest.file cmi, cmi, cmt, cmti)::st) & new_pcmis @ pcmis
  in
  f [] (map (fun cmi -> Oide_ident (Packpath.make [ap]), cmi) cmis)

let rec split_head = function
  | Oide_dot (Oide_ident s, n) when s.[0] = '{' -> s, Oide_ident n
  | Oide_dot (p, n) ->
      let s, p = split_head p in
      s, Oide_dot (p, n)
  | _ -> assert false
    
let rec put_head h = function
  | Oide_ident n -> Oide_dot (Oide_ident h, n)
  | Oide_dot (p, n) -> Oide_dot (put_head h p, n)
  | _ -> assert false
    
(* Heuristic path sorter.  Longer is better *) 
let sort_paths paths =
  let rec length = function
    | Oide_ident _ -> 1
    | Oide_dot (p,_) -> length p + 1
    | _ -> assert false
  in
  sort (fun p1 p2 -> - compare (length p1) (length p2)) paths

let ocaml_compiler_opam_build_dir = lazy begin
  let lazy sw = Package.sw in
  let d = sw.Opam.Switch.build_dir ^/ "ocaml" in
  if File.Test._d d then Some d else None
end
    
let traverse_packages apg =
  let aps = apg.Ocamlfind.Analyzed_group.packages in
  let opamo =
    match assoc_opt apg !!Package.opams_of_ocamlfind with
    | None -> None
    | Some [opam] -> Some opam
    | Some [] -> None
    | _ -> assert false (* XXX error handling *)
  in
  let srcdir =
    match opamo with
    | None -> 
        if exists Ocamlfind.Analyzed.is_base aps then
          (* This is from OCaml compiler. *)
          !!ocaml_compiler_opam_build_dir
        else
          None
    | Some opam -> Some (Opam.Package.build_dir opam)
  in
  let gs = 
    sort_then_group_by (fun (_,d,cmi,_,_) (_,d',cmi',_,_) ->
      compare (d, module_name cmi) (d', module_name cmi'))
    & concat_map (fun ap -> traverse_package srcdir apg ap) aps
  in
  let unify g =
    let paths = 
      let gs = sort_then_group_by (fun (_,p) (_,p') -> compare p p') & map (fun (path,_,_,_,_) -> split_head path) g in
      sort_paths
      & flip map gs & function
        | [] -> assert false
        | ((_,p)::_ as paths) ->
            let packs =
              Packpath.make_from_names
                & unique
                & concat_map (from_Some *< Packpath.parse) & map fst paths
            in
            put_head packs p
    in
    let (_,digest,cmi,_,_) = hd g in
    let cmt = find_map_opt id & map (fun (_,_,_,cmt,_) -> cmt) g in
    let cmti = find_map_opt id & map (fun (_,_,_,_,cmti) -> cmti) g in
    { paths; digest; cmi; cmt; cmti; ocamlfinds= map (fun x -> apg, x) aps; opam= opamo }
  in
  map unify gs

let traverse_packages, cache = memoize_gen & fun _self apg -> traverse_packages apg

let reset_cache () =
  Cache.reset_cache ();
  Hashtbl.clear cache

let out_of_opam_cmi_table = lazy begin
  (* no opam apgs *)
  let apgs = flip filter_map !!Package.opams_of_ocamlfind & function
    | apg, [] -> Some apg
    | _ -> None
  in
  !!% "@[<2>Scanning the following not-by-OPAM packages: @[%a@]@]@."
    Format.(list "@ " string) (map (fun apg -> apg.Ocamlfind.Analyzed_group.name) apgs);
  Hashtbl.create_with 101 & fun tbl ->
    flip iter apgs & fun apg ->
      flip iter (traverse_packages apg) & fun t ->
        (* Must use normalized basename *)
        Hashtbl.add tbl (String.uncapitalize_ascii & Filename.basename t.cmi, t.digest) t
end

let package_stamp ts =
  fst & uniq_dup_sorted compare & sort compare & map (fun t -> t.digest) ts

let warned_traverses = ref []
    
let guess p =
  let m = module_name p in
  let cmi = Filename.change_extension ~ext:".cmi" p in
  if not & File.Test._f cmi then failwithf "cmi file %s is not found" cmi;
  let d_cmi = Digest.file cmi in
  let traverse_and_find apgs =
    flip concat_map apgs & fun apg ->
      let ts = traverse_packages apg in
      filter (fun t -> m = module_name t.cmi && t.digest = d_cmi) ts
  in
  let default () =
    match Hashtbl.find_all !!out_of_opam_cmi_table (String.uncapitalize_ascii & Filename.basename cmi, d_cmi) with
    | [] ->
        let find ext p =
          let p = Filename.change_extension ~ext p in
          if File.Test._f p then Some p else None
        in
        [ { paths = [];
            digest = d_cmi;
            cmi = cmi;
            cmt = find ".cmt" p;
            cmti = find ".cmti" p;
            ocamlfinds = [];
            opam = None
          }
        ]
    | xs -> xs
  in
  match Opam.package_dir_of !!Package.sw p with
  | None -> default ()
  | Some (`OPAMBuild []) -> assert false
  | Some (`OPAMBuild (n::_)) -> 
      begin match
        filter (fun opam -> n = Opam.Package.name_version opam) !!Package.opam_packages
      with
      | [] ->
          (* .opam/<sw>/name.ver, but name.ver is not known to OPAM *)
          !!% "Warning: No opam package for %s" n;
          default ()
      | (_::_::_ as opams) -> 
          !!% "Warning: More than one opam packages for %s (%a)" n Format.(list "@ " string) (map (fun opam -> opam.Opam.Package.name) opams);
          default ()
      | [opam] ->
          match assoc_opt opam !!Package.ocamlfinds_of_opam with
          | None ->
              !!% "Warning: No OCamlFind packages for OPAM %s" opam.Opam.Package.name;
              default ()
          | Some [] ->
              !!% "Warning opam build %s has no ocamlfind package groups@." n;
              []
          | Some apgs ->
              traverse_and_find apgs |- fun res ->
                if res = [] then 
                  if add_if_not_mem (m,n) warned_traverses = `NewlyAdded then  
                    !!% "Warning: guess: returned [] for %s at traverse_and_find (with opam build %s) apgs=%a@."
                      m
                      n
                      Format.(list "@ " string)
                      (map (fun apg -> apg.Ocamlfind.Analyzed_group.name) apgs)
      end
  | Some (`OCamlFindLib []) -> assert false
  | Some (`OCamlFindLib (dir::_)) ->
      (* !!% "find_package %s : package dir %s@." p dir; *)
      let apgs = 
        filter (fun apg ->
          let dirs = Ocamlfind.Analyzed_group.dirs apg in
          flip exists dirs & fun d ->
            Opamfind.Utils.File.equal (d ^/ Filename.basename p) p
        ) !!Package.ocamlfind_package_groups
      in
      traverse_and_find apgs |- fun res ->
        if res = [] then
          !!% "Warning: guess: returned [] at traverse_and_find dir=%s apgs=%a@."
            dir
            Format.(list "@ " string) (map (fun apg -> apg.Ocamlfind.Analyzed_group.name) apgs)

  
let test packs =
  let apgs = match packs with
    | [] -> !!Package.ocamlfind_package_groups
    | xs ->
        flip filter !!Package.ocamlfind_package_groups (fun apg ->
          mem apg.Opamfind.Ocamlfind.Analyzed_group.name xs)
  in
  flip iter apgs & fun apg ->
    let ts = traverse_packages apg in
    !!% "@[<2>Modules of %s:@ @[%a@]@]@."
      apg.Ocamlfind.Analyzed_group.name
      Format.(list "@ " format) ts
