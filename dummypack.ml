(* Make a dummy package for test *)

open Spotlib.Spot
open List
open Opamfind
    
let create_package ~name ~dir = { Ocamlfind.Package.name; dir; defs = [] }
let create_group ~name p = { Ocamlfind.Group.name; packages = [p] }
let create_analyzed ~name ~dir cmis =
  let p = create_package ~name ~dir in
  { Ocamlfind.Analyzed.package = p
  ; archives = [] (* XXX ok? *)
  ; compilation_units = concat_map (from_Some *< Cmfile.get_compilation_units) cmis
  ; cmis
  }
let create_analyzed_group ~name ~dir cmis =
  { Ocamlfind.Analyzed_group.name
  ; packages = [ create_analyzed ~name ~dir cmis ]
  ; not_linked_cmis = []
  }
let create_analyzed_group_from_dir ~name ~dir =
  let cmis =
    Unix.Find.fold [dir] [] (fun st p ->
        match p#is_reg,  Filename.split_extension p#base with
        | true, (_, ".cmi") -> `Continue, p#path :: st
        | _ -> `Continue, st)
  in
  create_analyzed_group ~name ~dir cmis

      
