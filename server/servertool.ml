open Spotlib.Spot
open Cohttp_lwt_unix

(* XXX I can use Cohttp_lwt_unix.respond_file! *)

let if_modified_since h =
  let f s =
    (* prerr_endline ("< " ^ s); *)
    match Netdate.since_epoch & Netdate.parse s with
    | n ->
        (* !!% "< epoch %f@." n; *)
        `Ok n
    | exception _e -> `Error (`Date_parse_failed s)
  in
  Option.fmap f & Cohttp.Header.get h "If-Modified-Since"

(** memory stored file, but checks mtime to reload if the source is modified *)
let file_in_memory p =
  let open Unix in
  let mtime = ref 0.0 in
  let s = ref "" in
  let update () =
    let st = stat p in
    if !mtime = st.st_mtime then !s, !mtime
    else begin
      mtime := st.st_mtime; 
      s := from_Ok & File.to_string p;
      !s, !mtime
    end
  in
  ignore & update ();
  update

let respond_file_in_memory p ctype h =
  let file = file_in_memory p in
  let f = function
    | None ->
        (* prerr_endline "304"; *)
        Server.respond_string
          ~status: `Not_modified
          ~headers: (Cohttp.Header.of_list [])
          ~body: ""
          ()
    | Some (s, n) ->
        (* prerr_endline "200"; *)
        Server.respond_string
          ~status: `OK
          ~headers:(Cohttp.Header.of_list
                      [ "Content-Type",   ctype
                      ; "Content-Length", string_of_int & String.length s
                      ; "Last-Modified",  Netdate.mk_mail_date n
                      ])
          ~body: s
          ()
  in
  f & match if_modified_since h with
  | None -> Some (file ())
  | Some (`Error (`Date_parse_failed s)) ->
      !!% "If-Modified-Since: date parse failed: %s@." s;
      Some (file ())
  | Some (`Ok ims) ->
      let s, mt = file () in
      if mt <= ims then None
      else Some (s, mt)

