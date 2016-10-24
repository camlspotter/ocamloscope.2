open Spotlib.Spot
open List
open Utils
open Cmt_format

let rec get_load_path cmt =
  match cmt.cmt_annots with
  | Packed _ ->
      let cmis = Cm.get_packed cmt in
      unique & concat_map (fun cmi -> match Cm.guess cmi with
        | [] -> assert false
        | _::_::_ -> assert false (* ambiguous *)
        | [cm] ->
            begin match cm.Cm.cmt, cm.Cm.cmti with
            | Some x, _
            | _, Some x ->
                get_load_path (Cmt_format.read_cmt x)
            | _ -> assert false (* cmt required *)
            end)  cmis
  | _ -> 
      let thread_includes = 
        let args = Array.to_list cmt.cmt_args in
        if mem "-vmthread" args then [ !!Package.stdlib_dir ^/ "vmthreads" ]
        else if mem "-thread" args then [ !!Package.stdlib_dir ^/ "threads" ]
        else []
      in
      let cmt_loadpath = map (fun s -> cmt.cmt_builddir ^/ s) cmt.cmt_loadpath in
      cmt_loadpath @ thread_includes
  
(** Workaround of include path
    This is not perfect at all. But giving the dir of cmti 
    to the loading path seems to be fair.
    
    CR jfuruse:
    [cmt.cmt_loadpath] contains the build directory, but in relative.
    We need to make them absolute.
*)
let set_load_path cmt = Config.load_path := get_load_path cmt
