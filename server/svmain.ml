open Spotlib.Spot
open List
open Cohttp_lwt_unix

open Tyxml
module H = Html

let respond ~status html =
  let headers =
    Cohttp.Header.of_list
      ["Content-type", "text/html"
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
  let qstr, status, bs =
    match assoc_opt "q" qs with
    | None -> "", `Code 200, [ H.pcdata "Empty query" ]

    | Some [qstr] ->
        begin match Query.parse qstr with
        | [], _ -> qstr, `Bad_request, [ H.pcdata "Query parse failure" ]
        | qs, warns ->
            let warns = match warns with
              | [] -> []
              | _ ->
                  [ H.div ~a: [H.a_class ["warning"]]
                      & intersperse (H.br ())
                      & map (fun s -> H.pcdata ("Warning: " ^ s)) warns
                  ]
            in
            let res = Exn.catch_ & fun () -> Query.query data pspec qs in
            match res with  
            | `Ok [] ->
                qstr, `OK, warns @ [ H.pcdata "Empty result" ]
            | `Ok res ->
                qstr, `OK, warns @ [ Render.print_summary (Summary.group res) ]
            | `Error (`Exn e) -> 
                let str =
                  let trace = Exn.get_backtrace () in
                  !% "Uncaught exception: %s\nBacktrace:\n%s\n" (Exn.to_string e) trace
                in
                qstr, `Internal_server_error, warns @ [ H.pcdata str ]
        end
    | Some _ -> "", `Bad_request, [ H.pcdata "Illegal comma separated query" ]
  in

  if ngrok_mode then begin
    let headers = Cohttp.Header.of_list ["Content-type", "text/html"; "Access-Control-Allow-Origin", "*"] in
    Server.respond_string ~headers ~status ~body:(Render.html_elt_to_string (H.div bs)) ()
  end else
    respond ~status
    & H.html Render.oc_header
    & H.body & Render.query_form pspec qstr :: bs

let style_css       = Servertool.respond_file_in_memory "style.css" "text/css"
let ngrok_js        = Servertool.respond_file_in_memory "../ngrok/ngrok.js" "application/javascript"
let ngroklocal_html = Servertool.respond_file_in_memory "../ngrok/ngroklocal.html" "text/html"

let server ngrok_mode port data =
  let callback _conn req _body =
    let uri = Request.uri req in
    match Uri.path uri with
    | "/style.css"  -> style_css req.Request.headers 
    | "/ngrok.js" when ngrok_mode -> ngrok_js req.Request.headers
    | "/ngroklocal.html" when ngrok_mode -> ngroklocal_html req.Request.headers
    | "/" -> query ngrok_mode data & Uri.query uri
    | _ ->
        respond ~status:(`Code 404)
        & H.html Render.oc_header  
        & H.body [ H.span [ H.pcdata "404" ]]
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = 
  let port = ref 80 in
  let data_dir = ref "../out" in
  let ngrok_mode = ref false in
  Arg.parse 
    [ "--port", Arg.Int (fun x -> port := x), "port (default is 80)"
    ; "--data-dir", Arg.String (fun s -> data_dir := s), "data dir (default \"../out\")" 
    ; "--ngrok", Arg.Set ngrok_mode, "undocumented"
    ]
    (fun _ -> failwith "does not take any anonymous arguments")
    "server --port x --data_dir d";
  let data = Data.DB.unsafe_load (!data_dir ^/ "all.all") in
  ignore & Lwt_main.run & server !ngrok_mode !port data

