open Spotlib.Spot
open Utils
open List
open Data

module Siglink = struct
  open Outcometree
  
  let attach_type_ids fsigx =
    let with_types = ref 0 in
    let tyid = ref (-1) in
    let tbl = Hashtbl.create 1023 in
    let rev_fs_t = fold_left (fun rev_st (_,(r,_,_) as i) ->
      match Sig.the_type r with
      | None -> (i,None)::rev_st
      | Some ty ->
          incr with_types;
          match Hashtbl.find tbl ty with
          | exception Not_found ->
              incr tyid;
              Hashtbl.replace tbl ty !tyid;
              (i,Some !tyid)::rev_st
          | tyid ->
              (i, Some tyid)::rev_st) [] fsigx
    in
    let top_pack p =
      let rec f = function
        | Oide_dot (x, _) -> f x
        | Oide_apply (x, _) -> f x
        | Oide_ident s ->
            match Packpath.parse s with
            | None -> "predef" (* XXX I guess... *)
            | Some [] -> assert false
            | Some (x::_) -> top_package_name x
      in
      Hashcons.string & f p
    in
    let ts = Array.create (!tyid+1) Otyp_abstract in
    Hashtbl.iter (fun ty tyid -> Array.unsafe_set ts tyid ty) tbl;
    !!% "Siglink: %d unique types found in %d typed entries@." (!tyid+1) !with_types;
    let items = rev_map (fun (((k,i),(res,alias,vo)),tyid) ->
      {DB.kind=k;top_pack=top_pack i;path=i;res;tyid;alias; v= vo }) rev_fs_t
    in
    items, ts
end

let link_db data_dir =
  let files = sort compare & Unix.Find.fold [data_dir] [] & fun st path ->
    let p = path#path in
    if
      path#is_reg
      && match Filename.split_extension p with (_,".dat") -> true | _ -> false
    then `Continue, (p::st)
    else `Continue, st
  in
  !!% "Found %d dat files in %s@." (length files) data_dir; (* CR jfuruse: need to check uniqueness *)

  let dats = map Dat.load files in
  let packs = map (fun dat -> dat.Dat.name, dat.Dat.version) dats in
  let is = concat_map (fun dat -> dat.Dat.items) dats in
  !!% "Linking...@.";
  let items, types = Siglink.attach_type_ids is in
  !!% "Linked.@.";
  let db = { DB.items; top_types = types; packs; } in
  let p = data_dir ^/ "all.all" in
  !!% "Saving it to %s...@." p;
  DB.save p db;
  !!% "Saved.@.";
  db
