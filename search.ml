open Spotlib.Spot
open List

let rec search db pspec =
  Format.printf "Packs: %a@." (Ocaml.format_with Query.PackageSpec.ocaml_of_t) pspec;
  print_string "? ";
  let s = read_line () in
  let pspec' = match Directive.parse s with
    | Some (`Ok e) -> Directive.interpret db e pspec 
    | Some (`Error (`Exn exn)) ->
        !!% "Directive syntax error: %s@." (Exn.to_string exn);
        pspec
    | None ->
        let qs = Query.parse s in
        !!% "Query: %a@." (Ocaml.format_with [%derive.ocaml_of: Query.t list]) qs;
        begin match qs with
        | [] ->
            !!% "Query parse failure"
        | _ -> 
            let res = Query.query db pspec qs in
          (*
            let res = flip concat_map res (fun (d,xs) -> flip map xs (fun (ent,pt,tt) -> (d,ent,pt,tt))) in
            let res = map (fun (d,(ent,_),_,_) -> d,ent) res in
            flip iter res (fun (d,si) -> !!% "%d : %a@." d (Sigext.Print.sig_item true) si);
          *)
          
            Summary.group_and_print Format.stderr res;
          
            !!% "That's all@.";
        end;
        pspec
    | _ -> assert false
  in
  search db pspec'

let unsafe_load data_dir = Data.DB.unsafe_load (data_dir ^/ "all.all")

let do_search data_dir =
  let db = unsafe_load data_dir in
  search db & Query.PackageSpec.Vanilla []

let dump data_dir =
  let db = unsafe_load data_dir in
  flip iter db.items & fun i ->
    !!% "%a@."
      (Ocaml.format_with [%derive.ocaml_of: (Sig.k * Hump.path * Sig.res)])
      (i.kind, i.path, i.res)

