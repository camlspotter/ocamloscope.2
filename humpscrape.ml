open Spotlib.Spot
open Utils
open List
open Ocaml_conv.Default

let file_cmt top cmt_path =
  match Cmt_format.read cmt_path with
  | _cmio, None -> assert false
  | _cmio, Some cmt_infos -> 
      match cmt_infos.cmt_annots with
      | Partial_implementation _ | Partial_interface _ ->
          criticalf "Error: %s: partial cmt/cmti" cmt_path
      | Packed (_,ss) ->
          (* XXX I believe each s in ss is scraped as top.s and not as 
             top'.s for some other top'.  If it is wrong, eval will fail. *)
          (* 
             Creating an alias of

             let Global(<ocamlfind_pack>.<packing_mod>).N =
                   Global(<ocamlfind_pack>.<packing_mod>.N)
          *)
          Hump.ESignature (map (fun s ->
            let m = module_name s in
            (Sig.KModule, m), Hump.EGVar (Oide_dot (top,m))) ss)
      | Interface sg ->
          Cmt.set_load_path cmt_infos;
          let st = Humpext.signature top sg in
          Reset.typing ();
          (* !!% "@[%a@]@." Hump.format st; *)
          (* let st = H.eval [] st in *)
          (* !!% "==>@.@[%a@]@." Hump.format st; *)
          st
      | Implementation st -> 
          Cmt.set_load_path cmt_infos;
          (* Extract.test (`Structure st) *)
          let st = Humpext.structure top st in
          Reset.typing ();
          st
            
let test_cmt cmt =
  let cm = hd & Cm.guess cmt in (* XXX think about if many *)
  let top =
    if cm.Cm.paths = [] then
      Outcometree.Oide_dot (Oide_ident "NOTOP", module_name cmt) 
    else hd cm.Cm.paths
  in
  let module H = Humpeval.Make(struct
    let global_source _ = None
    let go_on_even_at_coercion_errors = true
  end) in
  let x = file_cmt top cmt in
  !!% "@[%a@]@." Hump.format x;
  let x = H.eval [] x in
  !!% "=> @[%a@]@." Hump.format x;
  x

let scrape_cmt m = match m.Cm.paths with
  | [] -> assert false
  | path::paths ->
      let cmt = match m.Cm.cmt with
        | None -> None
        | Some cmt ->
            !!% "scraping %s as %a...@." cmt Hump.format_path (hd m.Cm.paths);
            Some (file_cmt path cmt)
            |- fun _ -> !!% "scraped %s done@." cmt
      in
      let cmti = match m.Cm.cmti with
        | None -> None
        | Some cmti ->
            !!% "scraping %s as %a...@." cmti Hump.format_path (hd m.Cm.paths);
            Some (file_cmt path cmti)
            |- fun _ -> !!% "scraped %s done@." cmti
      in
      let e = match cmt, cmti with
        | Some cmt, Some cmti -> Some (Hump.ECoerce (cmt, cmti))
        | Some cmt, _ -> Some cmt
        | _, Some cmti -> Some cmti
        | _ ->
            !!% "@[<2>Error: No cmt/cmti found for@ @[%a@]]@." Cm.format m;
            !!% "@[If you are handling packages which are not installed by OPAM,@ adding --src-dir option to declare source directories explicitly may resolve the error.@]@.";
            criticalf "No cmt/cmti found for %s" m.Cm.cmi
      in
      (path, from_Some e)
      :: flip map paths (fun p ->
        (* p = path *)
        (p, Hump.(EModule (LocNone, EGVar path)))) (* XXX really? *)
        
let is_cached humpfile stamp =
  if File.Test._f humpfile then
    let s = Data.HumpFile.unsafe_load humpfile in
    if s.Data.HumpFile.stamp = stamp then Some s else None
  else None
    
let scrape_ocamlfind_package destdir dump apg =
  Hashcons.reset ();
  let open Opamfind in
  let open Ocamlfind in

  let path = destdir ^/ apg.Analyzed_group.name ^ ".hump" in

  match apg.Ocamlfind.Analyzed_group.name with
  | "predef" ->
      let humpfile =
        { Data.HumpFile.name = "predef"
        ; humps = [(Oide_ident "predef", Predefscrape.hump ())]
        ; stamp = []
        }
      in
      if not & File.Test._f path then begin
        !!% "Saving %s ...@." path;
        Data.HumpFile.save path humpfile;
        !!% "Saved.@.";
      end;
      humpfile

  | _ -> 

  let ms = Cm.traverse_packages apg in
  let stamp = Cm.package_stamp ms in

  match is_cached path stamp with
  | Some x -> x
  | None ->
    let humps = concat_map scrape_cmt ms in
    !!% "Scraped the compilation units of OCamlFind package group %s@."
      apg.Analyzed_group.name;
    if dump then 
      !!% "@[<2>HUMP@ @[%a@]@]@." (Ocaml.format_with [%derive.ocaml_of: (Hump.path * Hump.expr) list]) humps;

    let humpfile = Hashcons.humpfile { Data.HumpFile.name= apg.Analyzed_group.name; humps; stamp } in
    !!% "Saving %s...@." path;
    Data.HumpFile.save path humpfile;
    !!% "Saved.@.";

    Hashcons.reset ();
    humpfile
