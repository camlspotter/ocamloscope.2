open Spotlib.Spot
open Utils
open List
open Outcometree

let scrape_cmi x =
  let src = match Cm.guess x with
    | [] -> assert false
    | [src] -> src
    | _ -> assert false
  in
  (* !!% "@[<2>%s:@ source: @[%a@]@]@.@." x Zpack.format src; *)
  (* XXX need to fix the following! *)
  let top = match src.Cm.paths with
    | [] -> assert false
    | [ Oide_dot(Oide_ident p,s) ] -> Path.(Pdot (Pident (Ident.create_persistent p), s, 0))
    | _::_::_ -> assert false
    | _ -> assert false
  in

  (* XXX if packed, include path from cmt/cmti is useless *)
  let set_load_path () =
    let cmt = match src.Cm.cmt, src.Cm.cmti with
      | None, None ->
          !!% "ERROR: %s has no corresponding cmt/cmti, impossible to deduce its load path...@." x;
          assert false
      | Some x, _ -> x
      | _, Some x -> x
    in
    match Cmt_format.read cmt with
    | _, Some cmt_infos -> Cmt.set_load_path cmt_infos
    | _ -> assert false
  in
  set_load_path ();

  let cmi = src.cmi in
  let cmi_infos = Cmi_format.read_cmi cmi in
  let module P = Xprinttyp.Make(struct let rewrite = id end) in
prerr_endline "Actual scraping... (this may take long time.)";
  let fs = Hashcons.fsignature & Sigext.scrape (Some top) cmi_infos.Cmi_format.cmi_sign in
prerr_endline "Actual scrape done.";
  let top = Hashcons.out_ident & P.tree_of_path top in
  top, fs

let test_cmi cmi =
  let top, fs = scrape_cmi cmi in
  !!% "%a@.%a@." Xoprint.print_ident top Sig.format fs

open Opamfind

let is_cached sigfile stamp =
  if File.Test._f sigfile then
    let s = Data.SigFile.unsafe_load sigfile in
    if s.Data.SigFile.stamp = stamp then Some s else None
  else None
    
let scrape_ocamlfind_package destdir apg =
  let path = destdir ^/ apg.Ocamlfind.Analyzed_group.name ^ ".sig" in

  match apg.Ocamlfind.Analyzed_group.name with
  | "predef" ->
      let sigfile =
        { Data.SigFile.name = "predef"
        ; packs = [(Oide_ident "predef", Predefscrape.sig_ ())]
        ; stamp = []
        }
      in
      if not & File.Test._f path then begin
        !!% "Saving %s %d ...@." path (snd & Data.SigFile.sizes sigfile);
        Data.SigFile.save path sigfile;
        !!% "Saved.@.";
      end;
      sigfile

  | _ -> 

  let stamp = Cm.package_stamp & Cm.traverse_packages apg in

  match is_cached path stamp with
  | Some x -> x
  | None -> 
    Hashcons.reset ();
    let open Opamfind in
    let open Ocamlfind in

    let cmis = unique & concat_map (accessible_cmis apg) apg.Analyzed_group.packages
    in
    let packs = flip map cmis & fun cmi ->
      !!% "Scraping %s...@." cmi;
      scrape_cmi cmi
    in
  
    let sigfile = Hashcons.sigfile { Data.SigFile.name= apg.Analyzed_group.name; packs; stamp } in
    !!% "Saving %s %d ...@." path (snd & Data.SigFile.sizes sigfile);
    Data.SigFile.save path sigfile;
    !!% "Saved.@.";

    Hashcons.reset ();
    sigfile
