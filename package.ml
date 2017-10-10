open Spotlib.Spot
open Utils
open List
open Opamfind

let stdlib_dir, packages, sw = 
  let x = lazy (Opamfind.Assoc.init ()) in
  lazy (let (x,_,_) = !!x in x),
  lazy (let (_,y,_) = !!x in y),
  lazy (let (_,_,z) = !!x in z)

let predef = { Ocamlfind.Package.name = "predef"
             ; dir = ""
             ; defs = []
             }

let predef_ap = { Ocamlfind.Analyzed.package = predef
                ; archives = []
                ; compilation_units = []
                ; cmis = []
                }

let predef_apg = { Ocamlfind.Analyzed_group.name = "predef"
                 ; packages = [ predef_ap ]
                 ; not_linked_cmis = []
                 }
  
(*
let ocamlfind_package_groups = 
  flip map Ocamlfind.(group packages) & fun opg ->
    (opg, Ocamlfind.analyze_group ~stdlib_dir opg)
*)
let ocamlfind_package_groups = lazy (
  predef_apg ::
  flip map Ocamlfind.(group !!packages) ( fun opg ->
    Ocamlfind.analyze_group ~stdlib_dir:(!!stdlib_dir) opg )
)

let opam_packages = lazy (Opamlib.get_installed !!sw)

(* compute everything. probably inefficient *)
let ocamlfinds_of_opam, opams_of_ocamlfind = 
  let xy = lazy (
    let xs, ys = Assoc.associate !!ocamlfind_package_groups !!opam_packages in
    map (fun (op,ls) -> (op,map fst ls)) xs,
    ys
  )
  in
  lazy (fst !!xy),
  lazy (snd !!xy)
