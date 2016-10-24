open Spotlib.Spot

let dump_source_of_cmt_cmti ppf x = match Cmt_format.read x with
  | _, Some cmt_infos ->
      Cmt.set_load_path cmt_infos;
      (* CR jfuruse: bug. We should use sg from cmi instead of cmt/cmti,
         since the sg from cmt/cmti requires module scraping!!!
         *)
      begin match cmt_infos.cmt_annots with
      | Implementation s ->
          Pprintast.structure ppf (Untypeast.untype_structure s)
      | Interface s ->
          Pprintast.signature ppf (Untypeast.untype_signature s)
      | Packed (_, xs) ->
          Format.fprintf ppf "@[<2>%s: packed module of@ [ @[%a@] ]@]"
            x
            Format.(list ";@ " string) xs
      | _ -> failwithf "Error: %s: is not a complete structure or signature" x
      end
  | _ -> failwithf "Error: %s: No cmt_infos" x

let source = ref None

let dump_it () = match !source with
  | None -> !!% "Error: Dumpsource: no source specified@."
  | Some f ->
      !!% "Source:@.  @[%a@]@." dump_source_of_cmt_cmti f
