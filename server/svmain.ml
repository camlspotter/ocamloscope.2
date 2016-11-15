open Spotlib.Spot
open List
open Cohttp_lwt_unix

let bind = Lwt.bind

open Tyxml
module H = Html

let respond ~status html =
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "text/html"
      ; "Access-Control-Allow-Origin", "*" (* required for ngrok workaround *)
      ]
  in
  Server.respond_string ~headers ~status ~body:(Render.html_to_string html) ()

let query ngrok_mode data qs =
  let pspec =
    let open Query.PackageSpec in
    let split_by_space = String.split & function ' ' -> true | _ -> false in
    match assoc_opt "packtype" qs, assoc_opt "packs" qs with
    | (Some ["vanilla"] | None), Some [s] ->
        Vanilla (split_by_space s)
    | Some ["just"], Some [s] ->
        Just (split_by_space s)
    | Some ["allbut"], Some [s] ->
        All_but (split_by_space s)
    | _ -> Vanilla []
  in
  let qstr, status, bs, nitems, query_time, render_time =
    match assoc_opt "q" qs with
    | None ->
        "", `Code 200, [ H.pcdata "Empty query" ], None, None, None

    | Some [qstr] ->
        begin match Query.parse qstr with
        | [], _ ->
            qstr, `Bad_request, [ H.pcdata "Query parse failure" ], None, None, None
        | qs, warns ->
            let res, query_time = time Exn.catch_ & fun () -> Query.query data pspec qs in
            let (qstr, st, html, nitems), render_time = time (fun () ->
              let warns = match warns with
                | [] -> []
                | _ ->
                    [ H.div ~a: [H.a_class ["warning"]]
                        & intersperse (H.br ())
                        & map (fun s -> H.pcdata ("Warning: " ^ s)) warns
                    ]
              in
              match res with  
              | `Ok [] ->
                  qstr, `OK, warns @ [ H.pcdata "Empty result" ], Some 0
              | `Ok res ->
                  qstr, `OK, warns @ [ Render.print_summary (Summary.group res) ], Some (length res)
              | `Error (`Exn e) -> 
                  let str =
                    let trace = Exn.get_backtrace () in
                    !% "Uncaught exception: %s\nBacktrace:\n%s\n" (Exn.to_string e) trace
                  in
                  qstr, `Internal_server_error, warns @ [ H.pcdata str ], None) ()
            in
            qstr, st, html, nitems, Some query_time, Some render_time
        end

    | Some _ ->
        "", `Bad_request, [ H.pcdata "Illegal comma separated query" ], None, None, None
  in

  let bs =
    match query_time, render_time with
    | None, None -> bs
    | _ ->
        let qt = query_time // 0.0 in
        let rt = render_time // 0.0 in
        H.div ~a: [ H.a_class ["time"] ]
          [ H.pcdata (Printf.sprintf "(Query: %.2fsecs Render:%.2fsecs)" qt rt) ]
        :: bs
  in
  let respond = 
    if ngrok_mode then begin
      let headers = Cohttp.Header.of_list ["Content-type", "text/html"; "Access-Control-Allow-Origin", "*"] in
      Server.respond_string ~headers ~status ~body:(Render.html_elt_to_string (H.div bs)) ()
    end else 
      respond ~status
      & H.html Render.oc_header
      & H.body & Render.query_form pspec qstr :: bs
  in
  let t1 = Unix.gettimeofday () in
  let%m res = respond in
  let t2 = Unix.gettimeofday () in
  Lwt.return (res, (status, nitems, query_time, render_time, t2 -. t1))

let style_css       = Servertool.respond_file_in_memory "style.css" "text/css"
let ngrok_js        = Servertool.respond_file_in_memory "../ngrok/ngrok.js" "application/javascript"
let ngroklocal_html = Servertool.respond_file_in_memory "../ngrok/ngroklocal.html" "text/html"

let server ngrok_mode port data =
  let callback (conn : Server.conn) req _body =
    (* io_conn is useless when we use ngrok: we always have 127.0.0.1 *)
    let (io_conn : Conduit_lwt_unix.flow), _ = conn in
    let cli =
      let hs = (req : Request.t).headers in
      match Cohttp.Header.get hs "x-forwarded-for" with
      | Some adrs -> adrs (* ngrok *)
      | None -> Logdata.Client.source io_conn
    in
    let uri = Request.uri req in
    match Uri.path uri with
    | "/style.css"  -> style_css req.Request.headers 
    | "/ngrok.js" when ngrok_mode -> ngrok_js req.Request.headers
    | "/ngroklocal.html" when ngrok_mode -> ngroklocal_html req.Request.headers
    | "/" ->
        let q = Uri.query uri in
        Logdata.(log & Request { from= cli
                               ; query= q
                               }); 
        let%m (res, (st, nitems, query_time, render_time, response_time)) = query ngrok_mode data q in
        Logdata.(log & Response { from= cli
                                ; query= q
                                ; status= Cohttp.Code.code_of_status st
                                ; nitems
                                ; query_time
                                ; render_time
                                ; response_time
                                } );
        Lwt.return res
  | s ->
      Logdata.(log & Strange_path {from= cli; path= s });
      respond ~status:(`Code 404)
        & H.html Render.oc_header  
        & H.body [ H.span [ H.pcdata "404" ]]
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () =
  let port = ref 80 in
  let data_dir = ref "../out" in
  let log_file = ref None in
  let ngrok_mode = ref false in
  Arg.parse 
    [ "--port", Arg.Int (fun x -> port := x), "port (default is 80)"
    ; "--data-dir", Arg.String (fun s -> data_dir := s), "data dir (default \"../out\")"
    ; "--log-file", Arg.String (fun s -> log_file := Some s), "log file (default None)"
    ; "--ngrok", Arg.Set ngrok_mode, "undocumented"
    ]
    (fun _ -> failwith "does not take any anonymous arguments")
    "server --port x --data_dir d";
  let dbpath = !data_dir ^/ "all.all" in

  let () =
    match !log_file with
    | None -> ()
    | Some s ->
        Lwt_log_core.default :=
          Lwt_main.run & Lwt_log.file ~mode:`Append ~file_name: s ()
  in
  
  Logdata.(log & Server_started {port= !port});
  Logdata.(log & Note (!% "Loading db: %s" dbpath));
  try
    let data = Data.DB.unsafe_load dbpath in
    Logdata.(log & Note (!% "Loaded db: %s" dbpath));
    ignore & Lwt_main.run & server !ngrok_mode !port data
  with
  | exn ->
      Logdata.(log & Crash { exn= Printexc.to_string exn;
                             trace= Printexc.get_backtrace () })


